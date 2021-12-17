;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Homework 1, Problem 2 ==

; mystery : Image -> NonNegInteger
; This function returns the perimeter of the given image
(define (mystery x)
  (* 2
     (+ (image-height x)
        (image-width x))))

; TODO: replace "PURPOSE HERE" with a purpose
; statement that describes what the mystery
; function does.
;
; Importantly, don't simply translate the various
; functions/operations into English; instead,
; try to succinctly describe the real-world meaning
; of what the function is doing.