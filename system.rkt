#lang racket
(require racket/list
         racket/string
         racket/file
         racket/system)

(module+ test
  (require rackunit))

(provide comp_                     ; (comp_ stx) [MACRO]
         define-append             ; (define-append ... ) [MACRO]
         display-content           ; (display-content content filename)
         dup                       ; (dup str n)
         make-key-value-html-lines ; (make-key-value-html-lines inactive-item-tag active-item-tag active-item-index open-tag mid-tag close-tag pairs-list)
         map-append                ; (map-append ... ) [MACRO]
         map-append*               ; (map-append* ... ) [MACRO]
         multi-append              ; (multi-append open-tag mid-tag close-tag pairs-list)
         multi-append-list         ; (multi-append-list open-tag mid-tag close-tag pairs-list)
         multi-replace             ; (multi-replace l s)
         output-html)              ; (output-html content)


;;; purpose

; module provides main functions and macros to RAP
; ported from newLISP to Scheme by Dexter Santucci
; original newLISP code from myself at:
; https://github.com/DexterLagan/newstrap/blob/master/newstrap.lsp


;;; consts

(define *html-template*   "template.html")
(define *html-tag*        "<!-- page content -->")


;;; utility defs

;; replace html-tag in html-template by content and return the html
(define (output-html content)
  (display (string-replace (file->string *html-template*)
                           *html-tag*
                           content)))
; unit test
; (output-html "content")

;; create a temporary html page and point a browser to it
(define (display-content content filename)
  (let ((html (string-replace (file->string *html-template*) *html-tag* content)))
    (display-to-file html filename))
  (system*/exit-code "google-chrome" filename))
; path to Edge for when running on Windows
; TODO: implement OS detection and switching
; TODO: implement 
; "C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe" --profile-directory=Default

;; replace each pair of strings in the list l in the source s
;; i.e. (multi-replace '("some" "none" "something" "else") "some string, something"))
(define (multi-replace l s)
  (if (or (null? l) (null? s)) s
      (if (< (length l) 2) s
          (multi-replace (cddr l) (string-replace s (first l) (second l))))))
; unit test
(module+ test
  (check-equal? (multi-replace '("some" "none" "something" "else") "some string, something")
                "none string, nonething"))

;; string duplicator
(define (dup str n)
  (if (and (number? n)
           (> n 0))
      (apply string-append (for/list ((i n))
                             str))
      str))

; alternate implementation
(define (dup# str n)
  (if (and (number? n)
           (> n 0))
      (apply string-append (make-list n str))
      str))

; unit test
(module+ test
  (check-equal? (dup "<br>" 3)
                "<br><br><br>")
  (check-equal? (dup "<br>" -1)
                "<br>")
  (check-equal? (dup# "<br>" 3)
                "<br><br><br>")
  (check-equal? (dup# "<br>" -1)
                "<br>"))

;; multi-<br> tag generator
(define br
  (λ args
    (if (null? args)
        "<br>"
        (dup "<br>" (first args)))))
; unit test
(module+ test
  (check-equal? (br)
                "<br>")
  (check-equal? (br 3)
                "<br><br><br>")
  (check-equal? (br "o")
                "<br>")
  (check-equal? (br 0)
                "<br>")
  (check-equal? (br -1)
                "<br>"))


;;; system macros

; returns a function that composes parameters in order,
; using a placeholder _ for passing values between functions.
(define-syntax (comp_ stx)
  ; macro to compose functions passing an '_' parameter
  (syntax-case stx ()
    ((_ f1 ...)
     (with-syntax ([x-var (datum->syntax stx '_)])
       #'(apply compose1 (reverse (list (λ (x-var) f1) ...)))))))
; unit test
(module+ test
  (check-equal? ((comp_ (string-trim _)
                        (string-downcase _)
                        (string-replace _ " " "-")
                        ) "Hello World")
                "hello-world"))

;; macro defines a function that returns a string containing its arguments appended
;; between header and footer.
; usage:
; instead of :
; (define container
;     (lambda () (append "<div class='container' role='main'>" (aargs) "</div>")))
; you can write :
; (define-append container "<div class='container' role='main'>" "</div>")
(define-syntax define-append
  (syntax-rules ()
    ((define-append Func Header Footer)
     (define Func
       (λ args
         (string-append Header
                        (apply string-append args)
                        Footer))))))
; unit test
(module+ test
  (define-append container "<div class='container' role='main'>" "</div>")
  (check-equal? (container "some text")
                "<div class='container' role='main'>some text</div>"))

;; macro wraps each item in a list by enclosing tags
(define-syntax map-append
  (syntax-rules ()
    ((map-append Open-tag List Close-tag)
     (apply string-append (map (λ (s) (string-append Open-tag s Close-tag)) List)))))
; unit test
(module+ test
  (check-equal? (map-append "<small>" '("item1" "item2" "item3") "</small>")
                "<small>item1</small><small>item2</small><small>item3</small>"))

;; macro generates a string built out of a 2-dimentional list rows-list and the matching tags
(define-syntax map-append*
  (syntax-rules ()
    ((map-append* open-tag row-open-tag rows-list row-close-tag close-tag)
     (map-append open-tag 
                 (map (λ (cell) 
                        (map-append row-open-tag cell row-close-tag)) rows-list)
                 close-tag))))
; unit test
(module+ test
  (check-equal? (map-append* "<tr>" "<td>" '(("row1-column1" "row1-column2" "row1-column1")
                                             ("row2-column1" "row2-column2" "row2-column1")
                                             ("row3-column1" "row3-column2" "row3-column1")) "</td>" "</tr>")
                (string-append "<tr><td>row1-column1</td><td>row1-column2</td><td>row1-column1</td></tr>"
                               "<tr><td>row2-column1</td><td>row2-column2</td><td>row2-column1</td></tr>"
                               "<tr><td>row3-column1</td><td>row3-column2</td><td>row3-column1</td></tr>")))


;;; bootstrap grid system

(define-append container    "<div class='container' role='main'>" "</div>")
(define-append row          "<div class='row'>"                   "</div>")
(define-append column       "<div class='col-sm'>"                "</div>")
(define-append horizontal   "<p>"                                   "</p>") ; chaining controls horizontally


;;; visual elements

; displays a page title
(define-append title        "<div class='page-header'><h1>"  "</h1></div>")

;; function that takes a list of pairs of strings and appends them to opening, middle and closin tags
;; in the following way: open-tag s1 mid-tag s2 close-tag
(define (multi-append open-tag mid-tag close-tag pairs-list)
  (define (multi-append-rec l r)
    (if (null? l) r
        (if (< (length l) 2) r
            (string-append r (multi-append-rec (cddr l)
                                               (string-append open-tag
                                                              (list-ref l 0)
                                                              mid-tag
                                                              (list-ref l 1)
                                                              close-tag))))))
  (multi-append-rec pairs-list ""))
; unit test
(module+ test
  (check-equal? (multi-append "<open>" "<mid>" "<close>"
                              '("title1" "url1" "title2" "url2" "title3" "url3" "title4" "url4"))
                (string-append "<open>title1<mid>url1<close>"
                               "<open>title2<mid>url2<close>"
                               "<open>title3<mid>url3<close>"
                               "<open>title4<mid>url4<close>")))

;; same as multi-append but returns a list of strings for later processing
(define (multi-append-list open-tag mid-tag close-tag pairs-list)
  (define (multi-append-rec l r)
    (if (or (false? l)
            (null? l)) r
                       (if (< (length l) 2) r
                           (cons r
                                 (multi-append-rec (cddr l)
                                                   (string-append open-tag (list-ref l 0) mid-tag (list-ref l 1) close-tag))))))
  (flatten (cdr (multi-append-rec pairs-list '()))))
;(cdr (multi-append-rec pairs-list '()))) ; cuts initial empty list
; unit test
;(println (multi-append-list "<open>" "<mid>" "<close>" '("title1" "url1" "title2" "url2" "title3" "url3" "title4" "url4")))
(module+ test
  (check-equal? (multi-append-list "<open>" "<mid>" "<close>"
                                   '("title1" "url1" "title2" "url2" "title3" "url3" "title4" "url4"))
                '("<open>title1<mid>url1<close>"
                  "<open>title2<mid>url2<close>"
                  "<open>title3<mid>url3<close>"
                  "<open>title4<mid>url4<close>")))

;; generates a string containing a list of key-pair values englobed by opening, middle and closing tags
;; first three parameters are the opening, middle, closing tags as strings
;; last parameter is a flat list of key-pair values
(define (make-key-value-html-lines inactive-item-tag
                                   active-item-tag
                                   active-item-index
                                   open-tag
                                   mid-tag
                                   close-tag
                                   pairs-list)
  
  (let* ((links       (multi-append-list open-tag mid-tag close-tag pairs-list))
         (active-link (list-ref links active-item-index))
         (new-link    (string-replace active-link
                                      (string-append "<" inactive-item-tag ">")
                                      (string-append "<" inactive-item-tag " " active-item-tag ">")))
         (final-links (list-set links active-item-index new-link)))
    ;(apply string-append final-links)))
    final-links))

; unit test
(module+ test
  (check-equal? (make-key-value-html-lines "li"
                                           "class='active'"
                                           0
                                           "<li><a href='" "'>"
                                           "</a></li>"
                                           '("title1" "url1"
                                                      "title2" "url2"
                                                      "title3" "url3"
                                                      "title4" "url4"))
                '("<li class='active'><a href='title1'>url1</a></li>"
                  "<li><a href='title2'>url2</a></li>"
                  "<li><a href='title3'>url3</a></li>"
                  "<li><a href='title4'>url4</a></li>")))

;; displays a fixed navbar with the typical pop-up menu
;; parameters: title-link, title, active-item-index and any number of extra parameters such as:
;; link1 menu1 link2 menu2 link3 menu3 ...
(define navbar
  (λ args
    (define title-link        (first args))
    (define title             (second args))
    (define active-item-index (third args))
    (define other-params      (list-tail args 3))
    (let ((active-item-tag "class='active'"))
      (apply string-append (flatten (list
                                     "<nav class='navbar navbar-inverse navbar-fixed-top'>
      <div class='container'>
        <div class='navbar-header'>
          <button type='button' class='navbar-toggle collapsed' data-toggle='collapse' data-target='#navbar' aria-expanded='false' aria-controls='navbar'>
            <span class='sr-only'>Toggle navigation</span>
            <span class='icon-bar'></span>
            <span class='icon-bar'></span>
            <span class='icon-bar'></span>
          </button>
          <a class='navbar-brand' href='" title-link "'>" title "</a>
        </div>
        <div id='navbar' class='navbar-collapse collapse'>
          <ul class='nav navbar-nav'>"
                                          ; generating the following lines from the leftover arguments:
                                          ; <li class="active"><a href='#'>Home</a></li>
                                          ; <li><a href='#about'>About</a></li>
                                          ; <li><a href='#contact'>Contact</a></li>
                                          (make-key-value-html-lines "li" active-item-tag active-item-index "<li><a href='" "'>" "</a></li>" other-params)
                                          "</ul>
        </div><!--/.nav-collapse -->
      </div>
    </nav>"))))))
; unit test
(module+ test
  (check-equal? (navbar "#" "My Title" 0 "link1" "title1" "link2" "title2" "link3" "title3")
                (string-append  "<nav class='navbar navbar-inverse navbar-fixed-top'>\n"
                                "      <div class='container'>\n"
                                "        <div class='navbar-header'>\n"
                                "          <button type='button' class='navbar-toggle collapsed' data-toggle='collapse' data-target='#navbar' aria-expanded='false' aria-controls='navbar'>\n"
                                "            <span class='sr-only'>Toggle navigation</span>\n"
                                "            <span class='icon-bar'></span>\n"
                                "            <span class='icon-bar'></span>\n"
                                "            <span class='icon-bar'></span>\n"
                                "          </button>\n"
                                "          <a class='navbar-brand' href='#'>My Title</a>\n"
                                "        </div>\n"
                                "        <div id='navbar' class='navbar-collapse collapse'>\n"
                                "          <ul class='nav navbar-nav'>"
                                "<li class='active'><a href='link1'>title1</a></li>"
                                "<li><a href='link2'>title2</a></li>"
                                "<li><a href='link3'>title3</a></li>"
                                "</ul>\n"
                                "        </div><!--/.nav-collapse -->\n"
                                "      </div>\n"
                                "    </nav>")))

;; displays a large frame for important messages and promotions
(define (jumbotron title content)
  (string-append "<div class='jumbotron'><h1>" title "</h1><p>" content "</p></div>"))
; unit test
(module+ test
  (check-equal? (jumbotron "title" "content")
                "<div class='jumbotron'><h1>title</h1><p>content</p></div>"))


;;; button types

;; default, primary, success, info, warning, danger, link
(define (button type title)
  (string-append "<button type='button' class='btn btn-" type "'>" title "</button>"))
; unit test
(module+ test
  (check-equal? (button "type" "title")
                "<button type='button' class='btn btn-type'>title</button>"))

;; returns a link to the provided url showing the provided text
(define (link title url)
  (string-append "<a href='" url "'>" title "</a>"))
; unit test
(module+ test
  (check-equal? (link "title" "url")
                "<a href='url'>title</a>"))

;; bootstrap's fancy button link
(define (link-button title url)
  (string-append "<a href='" url "'><button type='button' class='btn btn-link'>" title "</button></a>"))
; unit test
(module+ test
  (check-equal? (link-button "title" "url")
                "<a href='url'><button type='button' class='btn btn-link'>title</button></a>"))

;; display a table. Rows is a list of rows, each row being a list of string cells.
(define (table headers rows)
  (string-append
   "<div class='row'>
       <div class='col-md-6'>
          <table class='table table-striped'>
            <thead>
              <tr>"
   (map-append "<th>" headers "</th>")
   "</tr>
            </thead>
            <tbody>"
   (map-append* "<tr>" "<td>" rows "</td>" "<tr>")
   "</tbody>
           </table>
          </div>
        </div>"))
; unit test
; TOOD

;; displays tabs, active-tab being the tab number to set as selected
;; support value pairs for the other parameters to describe tabs
(define tab-bar
  (λ args
    (define active-tab-index (first args)) ; link1 title1 link2 title2 link3 title3)
    (define other-params     (list-tail args 1))
    (let ((active-tab-tag "class='active'"))
      (apply string-append (flatten (list "<ul class='nav nav-tabs' role='tablist'>"
                     (make-key-value-html-lines "li role='presentation'" active-tab-tag active-tab-index "<li role='presentation'><a href='" "'>" "</a></li>" other-params)
                     ; genates the following lines:
                     ; <li role='presentation' class='active'><a href='" link1 "'>" title1 "</a></li>
                     ; <li role='presentation'><a href='" link2 "'>" title2 "</a></li>
                     ; <li role='presentation'><a href='" link3 "'>" title3 "</a></li>
                     "</ul>"))))))
; unit test
(module+ test
  (check-equal? (tab-bar 0 "link1" "title1" "link2" "title2" "link3" "title3")
                (string-append "<ul class='nav nav-tabs' role='tablist'>"
                               "<li role='presentation' class='active'><a href='link1'>title1</a></li>"
                               "<li role='presentation'><a href='link2'>title2</a></li>"
                               "<li role='presentation'><a href='link3'>title3</a></li>"
                               "</ul>")))

;; displays a thumbnail with alternative text
(define (thumbnail image alt)
  (string-append "<img src='" image "' class='img-thumbnail' alt='" alt "'>"))
; unit test
(module+ test
  (check-equal? (thumbnail "image" "alt")
                "<img src='image' class='img-thumbnail' alt='alt'>"))


;;; basic HTML visual elements

(define (text-field title id)
  (string-append title "<br><input type='text' name='" id "'>"))
; unit test
(module+ test
  (check-equal? (text-field "title" "id")
                "title<br><input type='text' name='id'>"))



; EOF
