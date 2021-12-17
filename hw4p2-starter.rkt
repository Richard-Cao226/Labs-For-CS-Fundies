;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 4, Problem 2 ==

; Consider the following data definition:

(define-struct processing [estimate])
(define-struct shipped [estimate])

; A PackageStatus is one of:
; - (make-processing String)
; - (make-shipped String)
; - "on the truck"
; - #true
; Interpretation: status of a package delivery,
; either processing (i.e., not yet shipped) with
; an expected ship date, already shipped with
; an expected delivery date, on the truck for
; delivery today, or already delivered (#true)

; TODO #1: finish the data design recipe for PackageStatus
(define PS-processing (make-processing "3/14/2021"))
(define PS-shipped (make-shipped "10/19/2020"))
(define PS-delivering "on the truck")
(define PS-delivered #true)

(define (PS-temp ps)
  (...
   (cond
     [(processing? ps) ... (processing-estimate ps) ...]
     [(shipped? ps) ... (shipped-estimate ps) ...]
     [(string? ps) ... ps ...]
     [(boolean? ps) ... ps ...]) ...))


; TODO #2: design the function package-update,
; which given a package label (e.g., "my new ipad"),
; number of items in the shipment (e.g., 1), and
; PackageStatus, and outputs a status update.

; Some example status updates include:
; - "my new ipad (1 item) is still processing and should ship on 3/14/2021"
; - "newest HP spinoff (2 items) has shipped and should arrive on 10/19/2020"
; - "tasty cookies (4 items) is on the truck for delivery today"
; - "red stapler (1 item) has been delivered"

; Hint: if you find that the right-hand side of your cond
; branches repeat a lot of the same work, you should move
; some of that work out of the cond (possibly into helper
; functions) so your code doesn’t repeat itself.

(check-expect (item-string 1) "1 item")
(check-expect (item-string 2) "2 items")

; item-string : NatNumber -> String
; returns a string with the number of items and singular or plural version of the word item
(define (item-string num-items)
  (if (= num-items 1) "1 item" (string-append (number->string num-items) " items")))


(check-expect (package-update "my new ipad" 1 PS-processing)
              "my new ipad (1 item) is still processing and should ship on 3/14/2021")
(check-expect (package-update "newest HP spinoff" 2 PS-shipped)
              "newest HP spinoff (2 items) has shipped and should arrive on 10/19/2020")
(check-expect (package-update "tasty cookies" 4 PS-delivering)
              "tasty cookies (4 items) is on the truck for delivery today")
(check-expect (package-update "red stapler" 1 PS-delivered)
              "red stapler (1 item) has been delivered")

; package-update : String NatNumber PackageStatus -> String
; returns a status update on the given order
(define (package-update label num-items ps)
  (cond
    [(processing? ps) (string-append label " (" (item-string num-items)
                                     ") is still processing and should ship on "
                                     (processing-estimate ps))]
    [(shipped? ps) (string-append label " (" (item-string num-items)
                                  ") has shipped and should arrive on "
                                  (shipped-estimate ps))]
    [(string? ps) (string-append label " (" (item-string num-items)
                                 ") is on the truck for delivery today")]
    [(boolean? ps) (string-append label " (" (item-string num-items)
                                  ") has been delivered")]))



