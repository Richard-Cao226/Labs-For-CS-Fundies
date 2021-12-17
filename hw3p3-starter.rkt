;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Homework 3, Problem 3 ==

; TODO: design the World program bit-face to allow
; someone to customize a face based upon a "menu"
; of options for eyes, mouth, and hair.
;
; You should have (at least) three options for eyes,
; three options for mouth, and three options for hair.
; Each time the user presses the "e" key, they should
; see the next option for eyes (cycling as necessary);
; same with "m" for mouth and "h" for hair. The
; bit-face function should supply initial values for
; each of these face features.
;
; You may draw these or use images from the web, but
; any combination (of eyes/mouth/hair) should be possible.
; You should NOT have a single representation of all
; possible combinations (since this will make it hard to
; add future options, such as ears, jewelry, makeup, ...).
; Instead, design data for each of the changing features.
; Be creative, but respectful :)

; As always, your functions should follow appropriate
; templates.


; Eyes is one of:
; - "square"
; - "circle"
; - "triangle"
; Interpretation: eyes of a face
(define EYES-SQUARE "square")
(define EYES-CIRCLE "circle")
(define EYES-TRIANGLE "triangle")

(define (eyes-temp eyes)
  (...
   (cond
     [(string=? eyes EYES-SQUARE) ...]
     [(string=? eyes EYES-CIRCLE) ...]
     [(string=? eyes EYES-TRIANGLE) ...]) ...))

; Mouth is one of:
; - "rectangle"
; - "circle"
; - "line"
; Interpretation: mouth of a face
(define MOUTH-RECTANGLE "rectangle")
(define MOUTH-CIRCLE "circle")
(define MOUTH-LINE "line")

(define (mouth-temp mouth)
  (...
   (cond
     [(string=? mouth MOUTH-RECTANGLE) ...]
     [(string=? mouth MOUTH-CIRCLE) ...]
     [(string=? mouth MOUTH-LINE) ...]) ...))

; Hair is one of:
; - "short"
; - "long"
; - "hat"
; Interpretation: hair of a face
(define HAIR-SHORT "short")
(define HAIR-LONG "long")
(define HAIR-HAT "hat")

(define (hair-temp hair)
  (...
   (cond
     [(string=? hair HAIR-SHORT) ...]
     [(string=? hair HAIR-LONG) ...]
     [(string=? hair HAIR-HAT) ...]) ...))

(define-struct face [eyes mouth hair])

; A Face is a (make-face Eyes Mouth Hair)
; Interpretation: a face has eyes, a mouth, and hair
(define INITIAL-FACE (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT))

(define (face-temp face)
  (... (eyes-temp (face-eyes face)) ...
       (mouth-temp (face-mouth face)) ...
       (hair-temp (face-hair face)) ...))

(check-expect (draw-eyes EYES-SQUARE)
              (overlay/offset (square 20 "solid" "black") 40 0 (square 20 "solid" "black")))
(check-expect (draw-eyes EYES-CIRCLE)
              (overlay/offset (circle 10 "solid" "black") 40 0 (circle 10 "solid" "black")))
(check-expect (draw-eyes EYES-TRIANGLE)
              (overlay/offset (triangle 20 "solid" "black") 40 0 (triangle 20 "solid" "black")))

; draw-eyes : Eyes -> Image
; draws the given eye shape
(define (draw-eyes eyes)
  (cond
    [(string=? eyes EYES-SQUARE)
     (overlay/offset (square 20 "solid" "black") 40 0 (square 20 "solid" "black"))]
    [(string=? eyes EYES-CIRCLE)
     (overlay/offset (circle 10 "solid" "black") 40 0 (circle 10 "solid" "black"))]
    [(string=? eyes EYES-TRIANGLE)
     (overlay/offset (triangle 20 "solid" "black") 40 0 (triangle 20 "solid" "black"))]))

(check-expect (draw-mouth MOUTH-RECTANGLE)
              (rectangle 60 30 "solid" "red"))
(check-expect (draw-mouth MOUTH-CIRCLE)
              (circle 20 "solid" "red"))
(check-expect (draw-mouth MOUTH-LINE)
              (line 50 0 "red"))

; draw-mouth : Mouth -> Image
; draws the given mouth shape
(define (draw-mouth mouth)
  (cond
    [(string=? mouth MOUTH-RECTANGLE) (rectangle 60 30 "solid" "red")]
    [(string=? mouth MOUTH-CIRCLE) (circle 20 "solid" "red")]
    [(string=? mouth MOUTH-LINE) (line 50 0 "red")]))

(check-expect (draw-hair HAIR-SHORT)
              (overlay/offset (line 0 10 "black") 20 0
                              (overlay/offset (line 0 10 "black") 15 0 (line 0 10 "black"))))
(check-expect (draw-hair HAIR-LONG)
              (overlay/offset (line 0 30 "black") 20 0
                              (overlay/offset (line 0 30 "black") 15 0 (line 0 30 "black"))))
(check-expect (draw-hair HAIR-HAT) (triangle 50 "solid" "blue"))
              
; draw-hair : Hair -> Image
; draws the given hairstyle
(define (draw-hair hair)
  (cond
    [(string=? hair HAIR-SHORT)
     (overlay/offset (line 0 10 "black") 20 0
                     (overlay/offset (line 0 10 "black") 15 0 (line 0 10 "black")))]
    [(string=? hair HAIR-LONG)
     (overlay/offset (line 0 30 "black") 20 0
                     (overlay/offset (line 0 30 "black") 15 0 (line 0 30 "black")))]
    [(string=? hair HAIR-HAT) (triangle 50 "solid" "blue")]))

; bit-face : Face -> Face
; draws a face based on a menu of options
(define (bit-face initial-face)
  (big-bang initial-face
    [to-draw draw-face]
    [on-key switch-option]))

(check-expect (draw-face INITIAL-FACE)
              (above (triangle 50 "solid" "blue")
                     (place-image (circle 20 "solid" "red") 80 110
                                  (place-image
                                   (overlay/offset
                                    (square 20 "solid" "black") 40 0
                                    (square 20 "solid" "black")) 80 50
                                                                 (circle 80 "outline" "black")))))

; draw-face : Face -> Image
; draws the given face
(define (draw-face face)
  (above (draw-hair (face-hair face))
         (place-image (draw-mouth (face-mouth face)) 80 110
                      (place-image (draw-eyes (face-eyes face)) 80 50
                                   (circle 80 "outline" "black")))))
  
(check-expect (switch-option (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT) "e")
              (make-face EYES-CIRCLE MOUTH-CIRCLE HAIR-HAT))
(check-expect (switch-option (make-face EYES-CIRCLE MOUTH-CIRCLE HAIR-HAT) "e")
              (make-face EYES-TRIANGLE MOUTH-CIRCLE HAIR-HAT))
(check-expect (switch-option (make-face EYES-TRIANGLE MOUTH-CIRCLE HAIR-HAT) "e")
              (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT))
(check-expect (switch-option (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT) "m")
              (make-face EYES-SQUARE MOUTH-LINE HAIR-HAT))
(check-expect (switch-option (make-face EYES-SQUARE MOUTH-LINE HAIR-HAT) "m")
              (make-face EYES-SQUARE MOUTH-RECTANGLE HAIR-HAT))
(check-expect (switch-option (make-face EYES-SQUARE MOUTH-RECTANGLE HAIR-HAT) "m")
              (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT))
(check-expect (switch-option (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT) "h")
              (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-SHORT))
(check-expect (switch-option (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-SHORT) "h")
              (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-LONG))
(check-expect (switch-option (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-LONG) "h")
              (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT))
(check-expect (switch-option (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT) "x")
              (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT))
  
; switch-option : Face Key -> Face
; changes a facial feature
(define (switch-option face key)
  (cond
    [(key=? key "e") (switch-eyes face)]
    [(key=? key "m") (switch-mouth face)]
    [(key=? key "h") (switch-hair face)]
    [else face]))

(check-expect (switch-eyes (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT))
              (make-face EYES-CIRCLE MOUTH-CIRCLE HAIR-HAT))
(check-expect (switch-eyes (make-face EYES-CIRCLE MOUTH-CIRCLE HAIR-HAT))
              (make-face EYES-TRIANGLE MOUTH-CIRCLE HAIR-HAT))
(check-expect (switch-eyes (make-face EYES-TRIANGLE MOUTH-CIRCLE HAIR-HAT))
              (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT))

; switch-eyes : Face -> Face
; switches eyes
(define (switch-eyes face)
  (cond
    [(string=? (face-eyes face) EYES-SQUARE)
     (make-face EYES-CIRCLE (face-mouth face) (face-hair face))]
    [(string=? (face-eyes face) EYES-CIRCLE)
     (make-face EYES-TRIANGLE (face-mouth face) (face-hair face))]
    [(string=? (face-eyes face) EYES-TRIANGLE)
     (make-face EYES-SQUARE (face-mouth face) (face-hair face))]))

(check-expect (switch-mouth (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT))
              (make-face EYES-SQUARE MOUTH-LINE HAIR-HAT))
(check-expect (switch-mouth (make-face EYES-SQUARE MOUTH-LINE HAIR-HAT))
              (make-face EYES-SQUARE MOUTH-RECTANGLE HAIR-HAT))
(check-expect (switch-mouth (make-face EYES-SQUARE MOUTH-RECTANGLE HAIR-HAT))
              (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT))

; switch-mouth : Face -> Face
; switches mouth
(define (switch-mouth face)
  (cond
    [(string=? (face-mouth face) MOUTH-RECTANGLE)
     (make-face (face-eyes face) MOUTH-CIRCLE (face-hair face))]
    [(string=? (face-mouth face) MOUTH-CIRCLE)
     (make-face (face-eyes face) MOUTH-LINE (face-hair face))]
    [(string=? (face-mouth face) MOUTH-LINE)
     (make-face (face-eyes face) MOUTH-RECTANGLE (face-hair face))]))

(check-expect (switch-hair (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT))
              (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-SHORT))
(check-expect (switch-hair (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-SHORT))
              (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-LONG))
(check-expect (switch-hair (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-LONG))
              (make-face EYES-SQUARE MOUTH-CIRCLE HAIR-HAT))

; switch-hair : Face -> Face
; switches hair
(define (switch-hair face)
  (cond
    [(string=? (face-hair face) HAIR-SHORT) (make-face (face-eyes face) (face-mouth face) HAIR-LONG)]
    [(string=? (face-hair face) HAIR-LONG) (make-face (face-eyes face) (face-mouth face) HAIR-HAT)]
    [(string=? (face-hair face) HAIR-HAT) (make-face (face-eyes face) (face-mouth face) HAIR-SHORT)]))

(bit-face INITIAL-FACE)