;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2p2-solution-richard) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 2, Problem 2 ==

; In each of the parts of this problem you will design
; Boolean functions with these restrictions:
;
; - in each part you are only allowed to use if,
;   the names of the parameters, #true, and #false
;   (though you may not need all of these);
;
; - you are not allowed to use an if that takes
;   the following form (if parameter #true #false),
;   since this is the same as the value of parameter;
;
; - the tests for your functions should cover ALL possible
;   input combinations for the parameters.
;
; And don't forget (for the rest of the class!), "designing" a function
; means to produce all 4 parts of the Design Recipe for functions!

; TODO: Design the function same? that takes two Boolean parameters
;       and returns true if either both are true or both are false.

(check-expect (same? #true #true) #true)
(check-expect (same? #true #false) #false)
(check-expect (same? #false #true) #false)
(check-expect (same? #false #false) #true)

; same? : Boolean Boolean -> Boolean
; Simulates XNOR gate
(define (same? b1 b2)
  (if b1 (if b2 #true #false) (if b2 #false #true)))

; TODO: Design the function non-agreement? that takes two Boolean parameters
;       and returns true if at least one of them is false.

(check-expect (non-agreement? #true #true) #false)
(check-expect (non-agreement? #true #false) #true)
(check-expect (non-agreement? #false #true) #true)
(check-expect (non-agreement? #false #false) #true)

; non-agreement? : Boolean Boolean -> Boolean
; Simulates NAND gate
(define (non-agreement? b1 b2)
  (if b1 (if b2 #false #true) #true))

; TODO: Design the function follow-directions that takes two Boolean parameters:
;       * if the first is false, it simply returns the second;
;       * if the first is true, it returns the opposite of the second

(check-expect (follow-directions #true #true) #false)
(check-expect (follow-directions #true #false) #true)
(check-expect (follow-directions #false #true) #true)
(check-expect (follow-directions #false #false) #false)

; follow-directions: Boolean Boolean -> Boolean
; If first parameter is false, second parameter is returned.
; If first parameter is true, opposite of second parameter is returned
(define (follow-directions b1 b2)
  (if b1 (if b2 #false #true) b2))