;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 3, Problem 1 ==

; Consider the following structure and data definition:

(define-struct element [name symbol num weight])

; An Element is a (make-element String String PosInteger PosReal)
; Interpretation: an element on the periodic table
; - name is the name of the element
; - symbol is the element's symbol
; - num is the atomic number
; - weight is the standard atomic weight

; TODO #1: List all signatures of all functions that are defined by
; this structure and data definition. Your signatures should
; be as precise as possible.

; make-element : String String PosInteger PosReal -> Element
; element-name : Element -> String
; element-symbol : Element -> String
; element-num : Element -> PosInteger
; element-weight : Element -> PosReal
; element? : Any -> Boolean


; TODO #2: Define at least three examples of Element
; (feel free to reference relevant sources, such as ...
; https://en.wikipedia.org/wiki/Periodic_table)
(define HYDROGEN (make-element "hydrogen" "H" 1 1.008))
(define LITHIUM (make-element "lithium" "Li" 3 6.941))
(define SODIUM (make-element "sodium" "Na" 11 22.990))


; TODO #3: Design the template for functions that consume an Element
(define (element-temp element)
  (... (element-name element) ...
       (element-symbol element) ...
       (element-num element) ...
       (element-weight element) ...))