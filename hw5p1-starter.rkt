;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw5p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 5, Problem 1 ==

; TODO #1: Design a data definition for a list of strings.

; A ListOfStrings (LOS) is one of:
; - '()
; - (cons String LOS)
; Interpretation: a list of strings

(define LOS-0 '())
(define LOS-1 (cons "Hello" LOS-0))
(define LOS-2 (cons "My name is Richard" LOS-1))
(define LOS-3 (cons "Hello" LOS-2))
(define LOS-4 (cons "My name is Richard" LOS-0))
(define LOS-5 (cons "Hello" LOS-3))

(define (los-temp los)
  (...
   (cond
     [(empty? los) ...]
     [(cons? los) ...
      (first los) ...
      (los-temp (rest los)) ...])))

; TODO #2: Design the function any-longer? that determines
; if any string in a supplied list of strings is longer
; than a supplied string.

(check-expect (any-longer? "String" LOS-0) #false)
(check-expect (any-longer? "String" LOS-1) #false)
(check-expect (any-longer? "String" LOS-2) #true)

; any-longer? : String LOS -> Boolean
; determines if any string in the given list of strings is longer than the given string
(define (any-longer? s los)
  (cond
    [(empty? los) #false]
    [(cons? los) (or (> (string-length (first los)) (string-length s)) (any-longer? s (rest los)))]))

; TODO #3: Design the function num-occurrences that counts
; the number of times a supplied string occurs within a
; list of strings.

(check-expect (num-occurrences "Hello" LOS-0) 0)
(check-expect (num-occurrences "Hello" LOS-1) 1)
(check-expect (num-occurrences "Hello" LOS-2) 1)
(check-expect (num-occurrences "Hello" LOS-3) 2)

; num-occurrences : String LOS -> NatNumber
; returns a count of the number of times the given string occurs in the given list of strings
(define (num-occurrences s los)
  (cond
    [(empty? los) 0]
    [(cons? los)
     (cond
       [(string=? s (first los)) (+ 1 (num-occurrences s (rest los)))]
       [else (num-occurrences s (rest los))])]))


; TODO #4: Design the function remove-occurrences that returns
; a list of strings with all occurrences of a supplied string
; removed from a supplied list of strings.

(check-expect (remove-occurrences "Hello" LOS-0) LOS-0)
(check-expect (remove-occurrences "Hello" LOS-1) LOS-0)
(check-expect (remove-occurrences "Hello" LOS-2) LOS-4)
(check-expect (remove-occurrences "Hello" LOS-5) LOS-4)

; remove-occurrences : String LOS -> LOS
; returns a list of strings with all occurrences of the given string in the given list of strings
; removed
(define (remove-occurrences s los)
  (cond
    [(empty? los) los]
    [(cons? los)
     (cond
       [(string=? s (first los)) (remove-occurrences s (rest los))]
       [else (cons (first los) (remove-occurrences s (rest los)))])]))