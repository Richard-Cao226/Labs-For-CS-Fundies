;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)
; == Homework 6, Problem 3 ==

; TODO #1: design the function item-counts that accepts a list of
; elements, a transformation function, and an equality function,
; and counts the distinct transformation results.

; The transformation function takes an element from the list and
; produces a result. The equality function takes two results of
; the transformation function and determines if they are the same.

; As a motivating example, consider counting words (supplied as
; strings): the transformation might convert each string to lower-case,
; the equality would then compare two strings to see if they are
; equal; and the resulting counts would be a list of pairings of
; distinct-lower-case words and how many times they appeared in the
; original list. SO... in case of Peter Piper
; (https://en.wikipedia.org/wiki/Peter_Piper), supplied as a list of
; strings without punctuation, you would learn that "peter" occurs 4
; times (as do "piper", "picked", "pickled", and "peppers"); "a"
; appears 3 times, and "if" appears 1 time (as does "where", "is",
; and "the").

; The function result should be a list, where each element is a
; pairing between a distinct transformation result and a count
; of how many times that result has occurred in the original list.
; You should design this data as a first step.

; You'll then design item-counts to consider each element of the
; supplied list in order. For each element, transform it and then
; add the result to the count. Adding is a bit tricky: first you
; check if that result has already been seen (and, if so, +1 to the
; associated counter); otherwise, add a new count pair of 1 to the end.

(define-struct ec (element count))

; An ElementCount is a (make-ec Any PosInteger)
; Interpretation: represents an element and a count of its occurrences

(define EC-PETER (make-ec "peter" 3))
(define EC-PIPER (make-ec "piper" 3))
(define EC-PICKED (make-ec "picked" 2))
(define EC-PICKLED (make-ec "pickled" 1))

(define (ec-temp ec)
  (... (ec-element ec) ...
       (ec-count ec) ...))

(check-expect (increment EC-PETER) (make-ec "peter" 4))

; increment : ElementCount -> ElementCount
; increments the count of the given ElementCount
(define (increment ec)
  (make-ec (ec-element ec) (add1 (ec-count ec))))

(check-expect (indexof "peter" (list (make-ec "picked" 2) (make-ec "peter" 3) (make-ec "piper" 3)
                                     (make-ec "pickled" 1)) string=? 0) 1)
(check-expect (indexof "peppers" (list (make-ec "picked" 2) (make-ec "peter" 3) (make-ec "piper" 3)
                                       (make-ec "pickled" 1)) string=? 0) -1)

; indexof : (X) X [List-of ElementCount] [X X -> Boolean] PosInteger -> Integer
; returns the index of the given element in the given list using the given equality function,
; or -1 if not found
(define (indexof element list f-equal i)
  (cond
    [(empty? list) -1]
    [(cons? list)
     (if (f-equal element (ec-element (first list)))
         i
         (indexof element (rest list) f-equal (add1 i)))]))

(check-expect (item-counts
               (list "picked" "Peter" "piper" "Peter" "piper" "picked" "pickled" "Peter" "piper")
               string-downcase string=? '())
              (list (make-ec "picked" 2) (make-ec "peter" 3)
                    (make-ec "piper" 3) (make-ec "pickled" 1)))

; item-counts :
; (X Y) [List-of X] [X -> Y] [Y Y -> Boolean] [List-of ElementCount] -> [List-of ElementCount]
; returns a list of ElementCounts representing the number of times that each element occurs
; in the given list
(define (item-counts list f-trans f-equal output)
  (cond
    [(empty? list) output]
    [(cons? list)
     (if (= (indexof (f-trans (first list)) output f-equal 0) -1) ; if element has not been seen yet
         (item-counts (rest list) f-trans f-equal ; add element to output list with count of 1
                      (append output (cons (make-ec (f-trans (first list)) 1) '())))
         (item-counts (rest list) f-trans f-equal ; otherwise, increment count of element
                      (list-update output
                                   (indexof (f-trans (first list)) output f-equal 0) increment)))]))

