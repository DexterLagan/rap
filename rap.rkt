#lang racket
(require "options.rkt")
(require "system.rkt")
(require racket/list
         racket/string
         racket/file
         racket/system
         compatibility/defmacro
         syntax/parse/define)
(require (for-template racket/base))

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
(define *config-filename* "rap.conf")












;;; main

; read configuration file
(read-options *config-filename*)
(define-options
  db-connection-name
  db-username
  db-password
  db-port
  db-name)



; EOF
