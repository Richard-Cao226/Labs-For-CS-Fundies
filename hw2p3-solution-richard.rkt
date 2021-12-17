;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2p3-solution-richard) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Homework 2, Problem 3 ==

; Your task is to design an interactive weekly exercise
; calendar. The program displays the current day and
; associated exercise, as allows the user to scroll forward/
; backward in the week by pressing the right/left arrow keys,
; respectively. You must do this according to the following
; sequence...


; TODO: Follow the Design Recipe for Weekday, which represents
;       the days of the week (Sunday - Saturday).

; A Weekday is one of:
; - "Sunday"
; - "Monday"
; - "Tuesday"
; - "Wednesday"
; - "Thursday"
; - "Friday"
; - "Saturday"
; Interpretation: seven days of the week

(define SUN "Sunday")
(define MON "Monday")
(define TUE "Tuesday")
(define WED "Wednesday")
(define THU "Thursday")
(define FRI "Friday")
(define SAT "Saturday")

(define (day-temp day)
  (...
   (cond
     [(string=? day SUN) ...]
     [(string=? day MON) ...]
     [(string=? day TUE) ...]
     [(string=? day WED) ...]
     [(string=? day THU) ...]
     [(string=? day FRI) ...]
     [(string=? day SAT) ...]) ...))

; TODO: Design a function exercise that returns an exercise given
;       a day of the week. Here is an example, but you are
;       free to come up with your own (as long as the activity
;       varies throughout the week, so you don't get bored):
;       - Sunday:    Climbing
;       - Monday:    Cardio
;       - Tuesday:   Upper Body + Core
;       - Wednesday: Cardio
;       - Thursday:  Lower Body + Core
;       - Friday:    Cardio
;       - Saturday:  Rest

(check-expect (exercise SUN) "Climbing")
(check-expect (exercise MON) "Cardio")
(check-expect (exercise TUE) "Upper Body + Core")
(check-expect (exercise WED) "Cardio")
(check-expect (exercise THU) "Lower Body + Core")
(check-expect (exercise FRI) "Cardio")
(check-expect (exercise SAT) "Rest")

; exercise : Weekday -> String
; returns the exercise on a given weekday
(define (exercise weekday)
  (cond
    [(string=? weekday SUN) "Climbing"]
    [(string=? weekday MON) "Cardio"]
    [(string=? weekday TUE) "Upper Body + Core"]
    [(string=? weekday WED) "Cardio"]
    [(string=? weekday THU) "Lower Body + Core"]
    [(string=? weekday FRI) "Cardio"]
    [(string=? weekday SAT) "Rest"]))

; TODO: Design the functions next-weekday and prev-weekday.
;       The former returns the weekday after that which
;       was supplied (and Monday comes after Sunday); the
;       the latter returns the weekday before that which
;       was supplied (and Sunday comes before Monday).

(check-expect (next-weekday SUN) MON)
(check-expect (next-weekday MON) TUE)
(check-expect (next-weekday TUE) WED)
(check-expect (next-weekday WED) THU)
(check-expect (next-weekday THU) FRI)
(check-expect (next-weekday FRI) SAT)
(check-expect (next-weekday SAT) SUN)

; next-weekday : Weekday -> Weekday
; returns the weekday after the given weekday
(define (next-weekday weekday)
  (cond
    [(string=? weekday SUN) MON]
    [(string=? weekday MON) TUE]
    [(string=? weekday TUE) WED]
    [(string=? weekday WED) THU]
    [(string=? weekday THU) FRI]
    [(string=? weekday FRI) SAT]
    [(string=? weekday SAT) SUN]))

(check-expect (prev-weekday SUN) SAT)
(check-expect (prev-weekday MON) SUN)
(check-expect (prev-weekday TUE) MON)
(check-expect (prev-weekday WED) TUE)
(check-expect (prev-weekday THU) WED)
(check-expect (prev-weekday FRI) THU)
(check-expect (prev-weekday SAT) FRI)

; prev-weekday : Weekday -> Weekday
; returns the weekday before the given weekday
(define (prev-weekday weekday)
  (cond
    [(string=? weekday SUN) SAT]
    [(string=? weekday MON) SUN]
    [(string=? weekday TUE) MON]
    [(string=? weekday WED) TUE]
    [(string=? weekday THU) WED]
    [(string=? weekday FRI) THU]
    [(string=? weekday SAT) FRI]))

; TODO: Finally, using these pieces, design the World program
;       exercise-calendar that displays the day and associated
;       exercise, as well as allowing the user to scroll forward/
;       backward in the week by pressing the right/left keys,
;       respectively. (Hint: in BSL, "right" and "left" are the
;       KeyEvent values you need to respond to; but what about
;       all the other keys?) You should supply to this function
;       the initial day of the week to show.

; exercise-calendar : Weekday -> Weekday
; displays weekday with exercise and allows the user to scroll forward/backward
(define (exercise-calendar initial-day)
  (big-bang initial-day
    [to-draw show-exercise]
    [on-key change-day]))

(check-expect (change-day SUN "right") MON)
(check-expect (change-day MON "right") TUE)
(check-expect (change-day TUE "right") WED)
(check-expect (change-day WED "right") THU)
(check-expect (change-day THU "right") FRI)
(check-expect (change-day FRI "right") SAT)
(check-expect (change-day SAT "right") SUN)
(check-expect (change-day SUN "left") SAT)
(check-expect (change-day MON "left") SUN)
(check-expect (change-day TUE "left") MON)
(check-expect (change-day WED "left") TUE)
(check-expect (change-day THU "left") WED)
(check-expect (change-day FRI "left") THU)
(check-expect (change-day SAT "left") FRI)

; change-day : Weekday + Key -> Weekday
; switches to previous or next day depending on if left or right was pressed
(define (change-day curr-day key)
  (cond
    [(key=? key "left") (prev-weekday curr-day)]
    [(key=? key "right") (next-weekday curr-day)]
    [else curr-day]))

(check-expect (show-exercise SUN) (overlay (text "Sunday - Climbing" 24 "black")
                                           (rectangle 500 100 "solid" "white")))
(check-expect (show-exercise MON) (overlay (text "Monday - Cardio" 24 "black")
                                           (rectangle 500 100 "solid" "white")))
(check-expect (show-exercise TUE) (overlay (text "Tuesday - Upper Body + Core" 24 "black")
                                           (rectangle 500 100 "solid" "white")))
(check-expect (show-exercise WED) (overlay (text "Wednesday - Cardio" 24 "black")
                                           (rectangle 500 100 "solid" "white")))
(check-expect (show-exercise THU) (overlay (text "Thursday - Lower Body + Core" 24 "black")
                                           (rectangle 500 100 "solid" "white")))
(check-expect (show-exercise FRI) (overlay (text "Friday - Cardio" 24 "black")
                                           (rectangle 500 100 "solid" "white")))
(check-expect (show-exercise SAT) (overlay (text "Saturday - Rest" 24 "black")
                                           (rectangle 500 100 "solid" "white")))

; show-exercise : Weekday -> Image
; displays given weekday with associated exercise
(define (show-exercise weekday)
  (overlay (text (string-append weekday " - " (exercise weekday)) 24 "black")
           (rectangle 500 100 "solid" "white")))

(exercise-calendar SUN)