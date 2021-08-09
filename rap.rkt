#lang racket/base
(require racket/list
         racket/string
         racket/file
         racket/system)

(module+ test
  (require rackunit))

;;; purpose

; to provide a minimum web app framework using boostrap 3.4 or newer

; see the following pages for sample content 
; http://bootstrapdocs.com/v3.3.4/docs/examples/theme/
; http://bootstrapdocs.com/v3.3.4/docs/getting-started/#examples

; use following links for CDN-hosted CSS, Theme, jQuery and Javascript :
; "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"
; "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap-theme.min.css"
; "https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"
; "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js"

;;; consts

(define *html-template*   "template.html")
(define *html-tag*        "<!-- page content -->")

;;; defs

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
; unit test
(module+ test
  (check-equal? (dup "<br>" 3)
                "<br><br><br>")
  (check-equal? (dup "<br>" -1)
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









;;; main


; EOF
