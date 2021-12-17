;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require racket/list)

; == Homework 9, Problem 1 ==

; We are going to continue working towards the project, designing several common
; functions for analyzing a text based upon the occurrence of words.

; Some parts will reference work you did in prior homework assignments - you may
; use your own solutions to those (ideally with any corrections from grading
; feedback) or the sample solutions on the course website.

; Unless otherwise specified in the problem, you should make appropriate use of
; list abstractions and local/lambda.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reading from Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; TODO #1: design the abstraction read-from-file-with-invalid, which checks if a
; file exists (returning an empty list if it does not) and otherwise calls a
; supplied function to return a list of strings. Use this function to design
; read-words-from-file (where the list of strings is each word in the file)
; and read-lines-as-strings-from-file (where the list is each line in the file).
; Finally, design the function read-lines-as-nums-from-file, which produces
; a list of numbers, assuming every line in a file can be converted to a number.
; Some tests have been provided for clarity.


(define BAD-FILE "BADFILE.badext")

(define PETER-PIPER
  (list "peter" "piper" "picked" "a" "peck" "of" "pickled" "peppers"
        "a" "peck" "of" "pickled" "peppers" "peter" "piper" "picked"
        "if" "peter" "piper" "picked" "a" "peck" "of" "pickled"
        "peppers" "where" "is" "the" "peck" "of" "pickled" "peppers"
        "peter" "piper" "picked"))

(define SCORE-WORDS (list "happy" "sad" "panda" "movie" "work"))
(define SCORE-VALUES (list 2.2 -5 0.2 1 -1))


(check-expect
 (read-from-file-with-invalid BAD-FILE read-words)
 '())

; read-from-file-with-invalid : String [String -> [List-of String]] -> [List-of String]
; returns empty list if given file doesn't exist, otherwise calls the given function to return a list
; of strings
(define (read-from-file-with-invalid file func)
  (if (file-exists? file) (func file) '()))


(check-expect
 (read-words-from-file "peter.txt")
 PETER-PIPER)

; read-words-from-file : String -> [List-of String]
; returns empty list if given file doesn't exist, otherwise returns a list of the words in the file
(define (read-words-from-file file)
  (read-from-file-with-invalid file read-words))


(check-expect
 (read-lines-as-strings-from-file "words.txt")
 SCORE-WORDS)

; read-lines-as-strings-from-file : String -> [List-of String]
; returns empty list if given file doesn't exist, otherwise returns a list of the lines in the file
(define (read-lines-as-strings-from-file file)
  (read-from-file-with-invalid file read-lines))


(check-expect
 (read-lines-as-nums-from-file "scores.txt")
 SCORE-VALUES)

; read-lines-as-nums-from-file : String -> [List-of Number]
; returns empty list if given file doesn't exist, otherwise returns a list of numbers,
; assuming every line in the file can be converted to a number.
(define (read-lines-as-nums-from-file file)
  (map string->number (read-lines-as-strings-from-file file)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; List Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; TODO #2: design the function first-k that returns a list of the first-k
; elements of a supplied list (or as many as are in the list, if that is fewer)
; Some tests have been provided for clarity. Do NOT use ISL list abstractions.

; Hint: think of both the list and number as complex inputs.



(check-expect (first-k '() 0) '())
(check-expect (first-k '() 3) '())
(check-expect (first-k (list 1 2 3) 3) (list 1 2 3))
(check-expect (first-k (list 1 2 3 4 5) 3) (list 1 2 3))
(check-expect (first-k (list "a" "b" "c") 4) (list "a" "b" "c"))

; first-k : (X) [List-of X] Nat -> [List-of X]
; returns a list of the first-k elements of a given list
(define (first-k loa k)
  (cond
    [(empty? loa) '()]
    [(cons? loa)
     (if (zero? k)
         '()
         (cons (first loa) (first-k (rest loa) (sub1 k))))]))


; TODO #3: design the function distinct-elements that returns the distinct
; elements that occur in a list according to an equality predicate. (Another
; way of stating this is that this function returns a new list where all the
; duplicates of the first list have been removed.) Some tests have been provided
; for clarity.

; Hints:
; - One good approach to this problem is to consider each element in the supplied
;   list and ask whether it occurs in the remainder of the list: if not, keep it;
;   otherwise don't.
; - The above approach basically works, but work some examples and you'll see that
;   the order is a bit off; to help, consider what would happen if the list were
;   analyzed in reverse order.
; - In HW8 you designed a function that will be quite useful here for finding an
;   item in a list via an equality predicate :)


(check-expect
 (distinct-elements '() =)
 '())

(check-expect
 (distinct-elements (list 1 2) =)
 (list 1 2))

(check-expect
 (distinct-elements (list 2 1 2) =)
 (list 2 1))

(check-expect
 (distinct-elements (list "a" "b" "b" "c" "b" "a" "b") string=?)
 (list "a" "b" "c"))

; distinct-elements : (X) [List-of X] [X X -> Boolean] -> [List-of X]
; returns a list of the distinct elements that occur in a given list according to a given
; equality predicate
(define (distinct-elements loa eq?)
  (local [; item-in-list? : (X) X [List-of X] -> Boolean
          ; checks if a given item is in a given list
          (define (item-in-list? val loa)
            (ormap (lambda (curr) (eq? curr val)) loa))

          ; build-distinct : (X) [List-of X] [List-of X] -> [List-of X]
          ; returns a list of the distinct elements that occur in a given list
          (define (build-distinct loa ans)
            (cond
              [(empty? loa) ans]
              [(cons? loa)
               (if (item-in-list? (first loa) ans)
                   (build-distinct (rest loa) ans)
                   (build-distinct (rest loa) (append ans (list (first loa)))))]))]
    (build-distinct loa '())))
    


; TODO #4: design the function words-score that accepts three lists:
; the first is a list of words (todo) to be scored, whereas the second (words)
; and third (scores) are assumed to be the same length and parallel (that is,
; the first element in the scores list is the score corresponding to the first
; word in the words list). The function should return the total score of all the
; todo words; if a todo word can't be found in the words list, its score is the
; supplied "default" score. Some tests have been provided for clarity.


(check-expect (words-score
               '()
               SCORE-WORDS SCORE-VALUES 3.14)
              0)

(check-expect (words-score
               (list "cabbage")
               SCORE-WORDS SCORE-VALUES 3.14)
              3.14)

(check-expect (words-score
               (list "what" "a" "happy" "movie")
               SCORE-WORDS SCORE-VALUES 0)
              3.2)

(check-expect (words-score
               (list "why" "so" "sad" "panda")
               SCORE-WORDS SCORE-VALUES 0)
              -4.8)

(check-expect (words-score
               (list "watching" "a" "movie" "at" "work")
               SCORE-WORDS SCORE-VALUES 0)
              0)

; words-score : [List-of String] [List-of String] [List-of Number] Number -> Number
; returns the total score of all the words in the first given list
(define (words-score todo words scores def)
  (cond
    [(empty? todo) 0]
    [(cons? todo)
     (if (number? (index-of words (first todo)))
         (+ (list-ref scores (index-of words (first todo)))
            (words-score (rest todo) words scores def))
         (+ def (words-score (rest todo) words scores def)))]))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Frequency Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO #5: design the function top-k-words that takes a list of words and
; returns the top-k words (by frequency of occurrance in the list) in
; alphabetical order. Some tests have been provided for clarity.

; This will take a few steps, most of which you've already done in this/other
; assignments; here is a suggested sequence, noting that local definitions
; are your friend :)

; 1. Count all the words (HW6)
; 2. Get the distinct counts (TODO #3)
; 3. Sort the distinct counts (biggest first)
; 4. Get the top-k of the sorted counts (TODO #2)
; 5. Get all words whose count is in the result of the previous step
; 6. Sort the resulting words alphabetically


(check-expect
 (top-k-words (list "baz" "bar" "baz" "foo") 1)
 (list "baz"))

(check-expect
 (top-k-words (list "baz" "bar" "baz" "foo") 2)
 (list "bar" "baz" "foo"))

; For reference (PETER-PIPER frequencies)...
; 4: peter piper picked peck of pickled peppers
; 3: a
; 1: if where is the

(check-expect
 (top-k-words PETER-PIPER 1)
 (list "of" "peck" "peppers" "peter" "picked" "pickled" "piper"))

(check-expect
 (top-k-words PETER-PIPER 2)
 (list "a" "of" "peck" "peppers" "peter" "picked" "pickled" "piper"))

; top-k-words : [List-of String] Nat -> [List-of String]
; returns the top-k words (by frequency of occurrance in the list) in alphabetical order
(define (top-k-words los k)
  (local [; A WordCount is a (make-wc String PosInteger)
          ; Interpretation: represents a word and a count of its occurrences
          ; - word is the word being counted
          ; - count is the count of the word
          (define-struct wc (word count))

          ; indexof : String [List-of WordCount] PosInteger -> Integer
          ; returns the index of the given word in the given list, or -1 if not found
          (define (indexof word lowc i)
            (cond
              [(empty? lowc) -1]
              [(cons? lowc)
               (if (string=? word (wc-word (first lowc)))
                   i
                   (indexof word (rest lowc) (add1 i)))]))
          
          ; word-counts : [List-of String] [List-of WordCount] -> [List-of WordCount]
          ; returns a list of WordCount representing the number of times that each word occurs
          ; in the given list
          (define (word-counts los output)
            (cond
              [(empty? los) output]
              [(cons? los)
               (if (= (indexof (first los) output 0) -1)
                   (word-counts (rest los) (append output (list (make-wc (first los) 1))))
                   (word-counts (rest los)
                                (list-update
                                 output (indexof (first los) output 0)
                                 (lambda (wc) (make-wc (wc-word wc) (add1 (wc-count wc)))))))]))

          ; get-counts : [List-of WordCount] -> [List-of Nat]
          ; returns a list of counts given a list of WordCount
          (define (get-counts lowc)
            (cond
              [(empty? lowc) '()]
              [(cons? lowc)
               (cons (wc-count (first lowc)) (get-counts (rest lowc)))]))
          
          ; get-top-words : [List-of Number] [List-of WordCount] -> [List-of String]
          ; returns a list of words with the top word counts
          (define (get-top-words lon lowc)
            (cond
              [(empty? lowc) '()]
              [(cons? lowc)
               (if (ormap (lambda (n) (= n (wc-count (first lowc)))) lon)
                   (cons (wc-word (first lowc)) (get-top-words lon (rest lowc)))
                   (get-top-words lon (rest lowc)))]))]
    
    (sort (get-top-words (first-k (sort (distinct-elements (get-counts (word-counts los '())) =) >) k)
                         (word-counts los '())) string<=?)))
