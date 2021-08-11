#lang racket/base
(require racket/string)
(module+ test
  (require rackunit))

;;; system macros

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
    (if (null? l) r
        (if (< (length l) 2) r
            (cons r (multi-append-rec (cddr l) (string-append open-tag (list-ref l 0) mid-tag (list-ref l 1) close-tag))))))
  (cdr (multi-append-rec pairs-list '()))) ; cuts initial empty list
; unit test
;(println (multi-append-list "<open>" "<mid>" "<close>" '("title1" "url1" "title2" "url2" "title3" "url3" "title4" "url4")))
(module+ test
  (check-equal? (multi-append-list "<open>" "<mid>" "<close>"
                                   '("title1" "url1" "title2" "url2" "title3" "url3" "title4" "url4"))
                '("<open>title1<mid>url1<close>"
                  "<open>title2<mid>url2<close>"
                  "<open>title3<mid>url3<close>"
                  .
                  "<open>title4<mid>url4<close>")))











; EOF
