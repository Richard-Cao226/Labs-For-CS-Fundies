;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 6, Problem 2 ==

; TODO #1: Design the function list-superlative that accepts a
; non-empty list and a value-function (which produces a number
; for each element of the list) and returns the first element
; that results in the maximal valuation. This is essentially an
; argmax and so you aren't allowed to use this function; you
; must produce a template-based, recursive solution for credit.
;
; As guiding examples...
; - given a list of the first three positive integers (1, 2, 3)
;   and the value-function identity (which just returns whatever
;   it is supplied), the function would return 3; however, if
;   the function were - (which negates the values), the function
;   would return 1.
; - given a list of (x, y) positions and posn-y as the
;   value-function, the function should return the position with
;   the largest y-coordinate
; - given a list of strings and string-length as the value-function,
;   the function should return the longest string

(define TEST-1 (list (make-posn 1 4) (make-posn 2 3) (make-posn 0 5)))
(define TEST-2 (list "hello" "my" "name" "is" "Richard"))

(check-expect (list-superlative TEST-1 posn-y) (make-posn 0 5))
(check-expect (list-superlative TEST-2 string-length) "Richard")

; list-superlative -> (X) [List-of X] [X -> Number]-> X
; given a list and a function that returns a number for every element in the list,
; returns the element that has the maximum valuation
(define (list-superlative list f)
  (cond
    [(empty? (rest list)) (first list)]
    [(cons? (rest list))
     (if (> (f (first list)) (f (first (rest list))))
         (list-superlative (cons (first list) (rest (rest list))) f)
         (list-superlative (rest list) f))]))