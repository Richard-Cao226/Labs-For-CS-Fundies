;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Homework 4, Problem 3 ==

; Consider the following data definitions:

; A Genre is one of:
; - "comedy"
; - "drama"
; - "action"
; - "education"
; Interpretation: genre for a video
(define COMEDY "comedy")
(define DRAMA "drama")
(define ACTION "action")
(define EDUCATION "education")

(define (genre-temp genre)
  (...
   (cond
     [(string=? COMEDY genre) ...]
     [(string=? DRAMA genre) ...]
     [(string=? ACTION genre) ...]
     [(string=? EDUCATION genre) ...]) ...))

(define-struct video [name duration hd? genre next])

; A StreamingQueue is one of:
; - #false
; - (make-video String PosInteger Boolean Genre StreamingQueue)
; Interpretation: either an empty queue
; or a video with a name, duration in minutes,
; whether it's available in HD, and it's genre.
(define SQ-empty #false)
(define SQ-video1 (make-video "Fundies: Part 1" 30 #true EDUCATION SQ-empty))
(define SQ-video2 (make-video "Fundies: Part 2" 40 #true EDUCATION SQ-video1))
(define SQ-video3 (make-video "Fundies: Part 3" 5 #false COMEDY SQ-video2))
(define SQ-only-short (make-video "Fundies: Part 3" 5 #false COMEDY SQ-empty))
(define SQ-ad-time1 (make-video "Fundies: Part 1" 31 #true EDUCATION SQ-empty))
(define SQ-ad-time2 (make-video "Fundies: Part 2" 41 #true EDUCATION SQ-ad-time1))
(define SQ-ad-time3 (make-video "Fundies: Part 3" 6 #false COMEDY SQ-ad-time2))

(define (SQ-temp sq)
  (...
   (cond
     [(boolean? sq) ... sq ...]
     [(video? sq) ...
      (video-name sq) ...
      (video-duration sq) ...
      (video-hd? sq) ...
      (genre-temp (video-genre sq)) ...
      (SQ-temp (video-next sq)) ...]) ...))


; TODO #1: complete the Design Recipe for Genre
; and StreamingQueue



; TODO #2: design the function queue-pic that produces
; an image of each title (with its duration) in the queue
; stacked vertically.

(check-expect (queue-pic SQ-empty) (overlay (text "Bottom of Queue" 40 "black")
                                            (rectangle 500 50 "outline" "black")))
(check-expect (queue-pic SQ-video3) (above (overlay
                                            (text "Fundies: Part 3 - 5 minutes"
                                                  40 "black")
                                            (rectangle 500 50 "outline" "black"))
                                           (above (overlay
                                                   (text "Fundies: Part 2 - 40 minutes"
                                                         40 "black")
                                                   (rectangle 500 50 "outline" "black"))
                                                  (above (overlay
                                                          (text "Fundies: Part 1 - 30 minutes"
                                                                40 "black")
                                                          (rectangle 500 50 "outline" "black"))
                                                         (overlay
                                                          (text "Bottom of Queue" 40 "black")
                                                          (rectangle 500 50 "outline" "black"))))))

; queue-pic : StreamingQueue -> Image
; displays a visualization of the given StreamingQueue
(define (queue-pic sq)
  (cond
    [(boolean? sq) (overlay (text "Bottom of Queue" 40 "black")
                            (rectangle 500 50 "outline" "black"))]
    [(video? sq) (above (overlay (text (string-append (video-name sq)
                                                      " - "
                                                      (number->string (video-duration sq))
                                                      " minutes")
                                       40 "black") (rectangle 500 50 "outline" "black"))
                        (queue-pic (video-next sq)))]))

; TODO #3: design the function all-hd? that determines
; if all the videos in the queue are available in HD.

(check-expect (all-hd? SQ-empty) #true)
(check-expect (all-hd? SQ-video2) #true)
(check-expect (all-hd? SQ-video3) #false)

; all-hd? : StreamingQueue -> Boolean
; checks if all videos in the given StreamingQueue are available in HD
(define (all-hd? sq)
  (cond
    [(boolean? sq) #true]
    [(video? sq) (and (video-hd? sq) (all-hd? (video-next sq)))]))


; TODO #4: design the function only-short that takes a
; queue and returns a new queue with only those videos
; in the original that are at most 12 minutes.

(check-expect (only-short SQ-empty) SQ-empty)
(check-expect (only-short SQ-video3) SQ-only-short)
(check-expect (only-short SQ-video2) SQ-empty)

; only-short : StreamingQueue -> StreamingQueue
; returns the given StreamingQueue with only the videos that are at most 12 minutes
(define (only-short sq)
  (cond
    [(boolean? sq) #false]
    [(and (video? sq) (<= (video-duration sq) 12))
     (make-video (video-name sq)
                 (video-duration sq)
                 (video-hd? sq)
                 (video-genre sq)
                 (only-short (video-next sq)))]
    [else (only-short (video-next sq))]))

; TODO #5: design the function add-ad-time that adds 1 minute
; to the duration of every video to account for requisite ads.

(check-expect (add-ad-time SQ-empty) SQ-empty)
(check-expect (add-ad-time SQ-video3) SQ-ad-time3)

; add-ad-time : StreamingQueue -> StreamingQueue
; returns the given StreamingQueue with the duration of all videos increased by 1
(define (add-ad-time sq)
  (cond
    [(boolean? sq) #false]
    [(video? sq) (make-video (video-name sq)
                             (+ (video-duration sq) 1)
                             (video-hd? sq)
                             (video-genre sq)
                             (add-ad-time (video-next sq)))]))

; TODO #6: design the function any-funny? that determines if any
; video in the queue is in the comedy genre.


(check-expect (funny? SQ-video3) #true)
(check-expect (funny? SQ-video2) #false)

; funny? : StreamingQueue -> Boolean
; checks if given video is a comedy
(define (funny? sq)
  (string=? (video-genre sq) COMEDY))

(check-expect (any-funny? SQ-empty) #false)
(check-expect (any-funny? SQ-video2) #false)
(check-expect (any-funny? SQ-video3) #true)

; any-funny? : StreamingQueue -> Boolean
; checks if there are any comedy videos in the given StreamingQueue
(define (any-funny? sq)
  (cond
    [(boolean? sq) #false]
    [(video? sq)
     (if (funny? sq) #true (any-funny? (video-next sq)))]))

