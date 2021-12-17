;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Homework 6, Problem 1 ==

; Consider the following functions:


; good-job : [List-of NonNegReal] -> [List-of NonNegReal]
; adds 20% to all supplied costs

(check-expect
 (good-job '())
 '())

(check-expect
 (good-job
  (cons 10 (cons 5 '())))
 (cons 12 (cons 6 '())))

;(define (good-job lon)
;  (cond
;    [(empty? lon) '()]
;    [(cons? lon)
;     (cons
;      (add-thanks (first lon))
;      (good-job (rest lon)))]))

(define (good-job lon)
  (abstract lon cons add-thanks '()))

; add-thanks : NonNegReal -> NonNegReal
; adds 20% to the supplied cost

(check-expect (add-thanks 10) 12)
(check-expect (add-thanks 5) 6)

(define (add-thanks cost)
  (* cost 1.2))


; total-length : [List-of String] -> Nat
; returns the total length of all strings in the list

(check-expect
 (total-length
  '())
 0)

(check-expect
 (total-length
  (cons "a" (cons "bb" '())))
 3)

;(define (total-length los)
;  (cond
;    [(empty? los) 0]
;    [(cons? los)
;     (+
;      (string-length (first los))
;      (total-length (rest los)))]))

(define (total-length los)
  (abstract los + string-length 0))

; TODO #1: abstract good-job and total-length.
; Be sure to re-define the functions using your new
; abstraction.

(check-expect (abstract (list 10 5) cons add-thanks '()) (list 12 6))
(check-expect (abstract (list "a" "bb") + string-length 0) 3)

; abstract : [List-of Any] [Any -> Any] [Any -> Any] Any -> Any
; returns something based on a given list with two given functions
(define (abstract list f1 f2 default)
  (cond
    [(empty? list) default]
    [(cons? list)
     (f1
      (f2 (first list))
      (abstract (rest list) f1 f2 default))]))


; TODO #2: use your new function to design the function
; acronym-image, which takes in a list of strings and
; visualizes them vertically stacked, with the first
; letter bolded (to highlight the acronym). The above/align
; function will be quite useful, as will the "modern" font
; (which is fixed-width, so all letters line up nicely).
; You can assume all supplied strings will have at least
; two letters.

(check-expect (acronym-image (list "oh" "my" "god"))
              (above/align
               "left"
               (beside (text/font "o" 20 "black" #f "modern" "normal" "bold" #f)
                       (text/font "h" 20 "black" #f "modern" "normal" "normal" #f))
               (above/align
                "left"
                (beside (text/font "m" 20 "black" #f "modern" "normal" "bold" #f)
                        (text/font "y" 20 "black" #f "modern" "normal" "normal" #f))
                (above/align
                 "left"
                 (beside (text/font "g" 20 "black" #f "modern" "normal" "bold" #f)
                         (text/font "od" 20 "black" #f "modern" "normal" "normal" #f))
                 (text/font "" 20 "black" #f "modern" "normal" "normal" #f)))))

; acronym-image : [List-of String] -> Image
; given a list of strings, vertically stacks them with the first letter of each string bolded
(define (acronym-image los)
  (abstract los align-left convert-string (text/font "" 20 "black" #f "modern" "normal" "normal" #f)))

(check-expect (convert-string "oh")
              (beside (text/font "o" 20 "black" #f "modern" "normal" "bold" #f)
                      (text/font "h" 20 "black" #f "modern" "normal" "normal" #f)))

; convert-string : String -> Image
; returns given string with first letter bolded
(define (convert-string s)
  (beside (text/font (substring s 0 1) 20 "black" #f "modern" "normal" "bold" #f)
          (text/font (substring s 1 (string-length s)) 20 "black" #f "modern" "normal" "normal" #f)))

(check-expect (align-left (text "hello" 20 "black") (text "hi" 20 "black"))
              (above/align "left" (text "hello" 20 "black") (text "hi" 20 "black")))

; align-left : Image Image -> Image
; vertically stacks given images and aligns them to the left
(define (align-left text1 text2)
  (above/align "left" text1 text2))
