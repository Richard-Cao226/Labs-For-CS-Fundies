;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 4, Problem 1 ==

; TODO: design the data necessary to represent a book, which can
; either be physical or electronic. All books have a title, author
; and publication year. Physical books are either paperback or
; hardcover, and have some number of pages. Electronic (e-books)
; have a format (pdf, epub, txt, azw, html) and a source URL.

; A Cover is one of:
; - "paperback"
; - "hardcover"
; Interpretation: type of cover for a physical book
(define PAPERBACK "paperback")
(define HARDCOVER "hardcover")

(define (cover-temp cover)
  (...
   (cond
     [(string=? cover PAPERBACK) ...]
     [(string=? cover HARDCOVER) ...]) ...))

; A Format is one of:
; - "pdf"
; - "epub"
; - "txt"
; - "azw"
; - "html"
(define PDF "pdf")
(define EPUB "epub")
(define TXT "txt")
(define AZW "azw")
(define HTML "html")

(define (format-temp format)
  (...
   (cond
     [(string=? format PDF) ...]
     [(string=? format EPUB) ...]
     [(string=? format TXT) ...]
     [(string=? format AZW) ...]
     [(string=? format HTML) ...]) ...))

(define-struct physical [title author year cover num-pages])
(define-struct electronic [title author year format url])

; A Book is one of:
; - (make-physical String String PosInteger Cover PosInteger)
; - (make-electronic String String PosInteger Format String)
; Interpretation: a book with a title, author, and publication year,
; either physical (with a cover type and a number of pages)
; or electronic (with a format and a source URL)
(define PHYSICAL-BOOK (make-physical "CS Fundies" "Richard" 2021 PAPERBACK 10))
(define ELECTRONIC-BOOK (make-electronic "CS Fundies" "Richard" 2021 PDF "example.com"))

(define (book-temp book)
  (...
   (cond
     [(physical? book) ...
      (physical-title book) ...
      (physical-author book) ...
      (physical-year book) ...
      (cover-temp (physical-cover book)) ...
      (physical-num-pages book) ...]
     [(electronic? book) ...
      (electronic-title book) ...
      (electronic-author book) ...
      (electronic-year book) ...
      (format-temp (electronic-format book)) ...
      (electronic-url book) ...]) ...))