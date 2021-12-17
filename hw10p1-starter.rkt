;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 10, Problem 1 ==

; Consider a gradebook for a class.

; In order to represent how individual assignments lead to a class
; grade, you can think that there are assignment columns (each
; with a name and total points) and calculated columns (each with
; a name, operation, and columns over which to operate).
; Operations include taking the simple average, dropping some
; number of lowest values, or weighting the columns.

; For example...
; - a total column, representing a weighted average over...
;   - a homework column (worth 30% of total), representing a simple average over...
;     - 4 assignment columns (hw1-4; each out of 20pts)
;   - a project column (out of 100pts; worth 50% of total)
;   - a quizzes column (worth 20% of total), representing a weighted average over...
;     - a pre-class quizzes column (worth 20% of quizzes; with the lowest dropped), broken into...
;       - 3 columns (pcq1-3; each out of 5pts)
;     - an in-class quizzes column (worth 80% of quizzes; with the lowest dropped), broken into...
;       - 3 columns (icq1-3; each out of 10pts)


; TODO #1: using the above description, design the data for a Gradebook.
; You should represent the gradebook description above with your examples.

; An Operation is one of:
; - #false
; - (list-of Float)
; - PosInt
; Interpretation: operation to perform on gradebook columns including taking the simple average,
; taking the weighted average, or dropping some number of lowest grades

(define TOTAL-WEIGHT (list 0.3 0.5 0.2))
(define SIMPLE-AVG #false)
(define QUIZ-WEIGHT (list 0.2 0.8))
(define QUIZ-INVALID-WEIGHT1 (list 0.1 0.8 0.1))
(define QUIZ-INVALID-WEIGHT2 (list 1.8 -0.8))
(define QUIZ-INVALID-WEIGHT3 (list 0.3 0.8))
(define PCQ-DROP 1)
(define ICQ-DROP 1)
(define INVALID-DROP 3)

(define (op-temp op)
  (...
   (cond
     [(boolean? op) ...]
     [(list? op) ...
      (cond
        [(empty? op) ...]
        [(cons? op) ...
         (first op) ...
         (rest op) ...])]
     [(number? op) ...])))


(define-struct asgmt (name total))
(define-struct calc (name op cols))

; A Gradebook is one of:
; - (make-asgmt String PosInt)
; - (make-calc String Operation [List-of Gradebook])
; Interpretation: an assignment column with a name and total points; or a calculated column with a
; name, operation, and columns over which to operate

(define HW1 (make-asgmt "hw1" 20))
(define HW2 (make-asgmt "hw2" 20))
(define HW3 (make-asgmt "hw3" 20))
(define HW4 (make-asgmt "hw4" 20))
(define HW-CALC (make-calc "homework" SIMPLE-AVG (list HW1 HW2 HW3 HW4)))
(define PROJ (make-asgmt "project" 100))
(define PCQ1 (make-asgmt "pcq1" 5))
(define PCQ2 (make-asgmt "pcq2" 5))
(define PCQ3 (make-asgmt "pcq3" 5))
(define PCQ-CALC (make-calc "pre-class quizzes" PCQ-DROP (list PCQ1 PCQ2 PCQ3)))
(define ICQ1 (make-asgmt "icq1" 10))
(define ICQ2 (make-asgmt "icq2" 10))
(define ICQ3 (make-asgmt "icq3" 10))
(define ICQ-CALC (make-calc "in-class quizzes" ICQ-DROP (list ICQ1 ICQ2 ICQ3)))
(define QUIZZES-CALC (make-calc "quizzes" QUIZ-WEIGHT (list PCQ-CALC ICQ-CALC)))
(define TOTAL (make-calc "total" TOTAL-WEIGHT (list HW-CALC PROJ QUIZZES-CALC)))

; when a column has empty name
(define TOTAL-EMPTY (make-calc "" TOTAL-WEIGHT (list HW-CALC PROJ QUIZZES-CALC)))

; when the number of dropped grades is not less than the number of columns
(define PCQ-INVALID-DROP (make-calc "pre-class quizzes" INVALID-DROP (list PCQ1 PCQ2 PCQ3)))
(define QUIZZES-INVALID-DROP (make-calc "quizzes" QUIZ-WEIGHT (list PCQ-INVALID-DROP ICQ-CALC)))
(define TOTAL-INVALID-DROP (make-calc "total" TOTAL-WEIGHT (list HW-CALC PROJ QUIZZES-INVALID-DROP)))

; when the number of weights is unequal to the number of columns
(define QUIZZES-INVALID-WEIGHT1 (make-calc "quizzes" QUIZ-INVALID-WEIGHT1 (list PCQ-CALC ICQ-CALC)))
(define TOTAL-INVALID-WEIGHT1 (make-calc "total" TOTAL-WEIGHT (list HW-CALC PROJ
                                                                    QUIZZES-INVALID-WEIGHT1)))

; when one of the weights is not positive
(define QUIZZES-INVALID-WEIGHT2 (make-calc "quizzes" QUIZ-INVALID-WEIGHT2 (list PCQ-CALC ICQ-CALC)))
(define TOTAL-INVALID-WEIGHT2 (make-calc "total" TOTAL-WEIGHT (list HW-CALC PROJ
                                                                    QUIZZES-INVALID-WEIGHT2)))

; when the weights don't add up to 100%
(define QUIZZES-INVALID-WEIGHT3 (make-calc "quizzes" QUIZ-INVALID-WEIGHT3 (list PCQ-CALC ICQ-CALC)))
(define TOTAL-INVALID-WEIGHT3 (make-calc "total" TOTAL-WEIGHT (list HW-CALC PROJ
                                                                    QUIZZES-INVALID-WEIGHT3)))

(define (gb-temp gb)
  (...
   (cond
     [(asgmt? gb) ...
      (asgmt-name gb) ...
      (asgmt-total gb) ...]
     [(calc? gb) ...
      (calc-name gb) ...
      (op-temp (calc-op gb)) ...
      (logb-temp (calc-cols gb)) ...])))

(define (logb-temp logb)
  (...
   (cond
     [(empty? logb) ...]
     [(cons? logb) ...
      (gb-temp (first logb)) ...
      (logb-temp (rest logb)) ...])))

; TODO #2: design the function valid-gradebook, which makes sure...
; - the names of all columns aren't empty
; - the number of dropped grades is always smaller
;   than the number of columns in that calculated column
; - the weights in a weighted average make sense: there is
;   one for each column, they are all positive, and they
;   add up to 100%

(check-expect (valid-gradebook TOTAL) #true)
(check-expect (valid-gradebook TOTAL-EMPTY) #false)
(check-expect (valid-gradebook TOTAL-INVALID-DROP) #false)
(check-expect (valid-gradebook TOTAL-INVALID-WEIGHT1) #false)
(check-expect (valid-gradebook TOTAL-INVALID-WEIGHT2) #false)
(check-expect (valid-gradebook TOTAL-INVALID-WEIGHT3) #false)

; valid-gradebook : Gradebook -> Boolean
; checks if the given gradebook is valid
(define (valid-gradebook gb)
  (local [; valid-gradebook-gb : Gradebook -> Boolean
          ; checks if all assignment and calculated columns are valid in the given gradebook
          (define (valid-gradebook-gb gb)
            (cond
              [(asgmt? gb) (not (string=? (asgmt-name gb) ""))]
              [(calc? gb)
               (check-calc (calc-name gb) (calc-op gb) (calc-cols gb))]))

          ; check-calc : String Operation [List-of Gradebook] -> Boolean
          ; helper function that checks if an operation is valid for a calculated column,
          ; and also checks the rest of the columns
          (define (check-calc name op cols)
            (cond
              [(boolean? op)
               (and (not (string=? name ""))
                    (valid-gradebook-logb cols))]
              [(list? op)
               (and (not (string=? name ""))
                    (= (length op) (length cols))
                    (andmap positive? op)
                    (= (foldr + 0 op) 1)
                    (valid-gradebook-logb cols))]
              [(number? op)
               (and (not (string=? name ""))
                    (< op (length cols))
                    (valid-gradebook-logb cols))]))

          ; valid-gradebook-logb : [List-of Gradebook] -> Boolean
          ; checks if all assignment and calculated columns are valid in the given list of gradebooks
          (define (valid-gradebook-logb logb)
            (andmap valid-gradebook-gb logb))]
    
    (valid-gradebook-gb gb)))