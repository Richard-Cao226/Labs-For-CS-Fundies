;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 3, Problem 2 ==

; TODO: design the function command-point that accepts a command
; ("left", "right", "up", "down"), an (x, y) position,
; and a non-negative distance and produces a new point moving
; according to the command and distance. For example, if you
; start at (2, 3), the command "left" 1 should produce (1, 3),
; whereas "down" 2.2 would produce (2, 5.2).
;
; You should design whatever data is necessary for this function
; and, as always, remember to follow the templates in your function
; implementation (hint: if your command-point function is getting
; large and repetitive, you are likely not following the templates
; and need a helper).

; A Command is one of:
; - "left"
; - "right"
; - "up"
; - "down"
; Interpretation: a direction
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")

(define (command-temp command)
  (...
   (cond
     [(string=? command LEFT) ...]
     [(string=? command RIGHT) ...]
     [(string=? command UP) ...]
     [(string=? command DOWN) ...]) ...))


; A Position is a (make-posn Real Real)
; Interpretation: a 2D location
(define POS-INITIAL (make-posn 2 3))
(define POS-LEFT (make-posn 1 3))
(define POS-RIGHT (make-posn 3 3))
(define POS-UP (make-posn 2 2))
(define POS-DOWN (make-posn 2 4))

(define (posn-temp position)
  (... (posn-x position) ...
       (posn-y position) ...))

(check-expect (change-x POS-INITIAL 1) POS-RIGHT)
(check-expect (change-x POS-INITIAL -1) POS-LEFT)

; change-x : Position Number -> Position
; changes x coordinate of given position by the given distance
(define (change-x pos dist)
  (make-posn (+ (posn-x pos) dist) (posn-y pos)))

(check-expect (change-y POS-INITIAL 1) POS-DOWN)
(check-expect (change-y POS-INITIAL -1) POS-UP)

; change-y : Position Number -> Position
; changes y coordinate of given position by the given distance
(define (change-y pos dist)
  (make-posn (posn-x pos) (+ (posn-y pos) dist)))

(check-expect (command-point LEFT POS-INITIAL 1) POS-LEFT)
(check-expect (command-point RIGHT POS-INITIAL 1) POS-RIGHT)
(check-expect (command-point UP POS-INITIAL 1) POS-UP)
(check-expect (command-point DOWN POS-INITIAL 1) POS-DOWN)

; command-point : Command, Position, Non-negative Number -> Position
; accepts a command and distance and updates the given position to a new position
(define (command-point command pos dist)
  (cond
    [(string=? command LEFT) (change-x pos (* dist -1))]
    [(string=? command RIGHT) (change-x pos dist)]
    [(string=? command UP) (change-y pos (* dist -1))]
    [(string=? command DOWN) (change-y pos dist)]))
