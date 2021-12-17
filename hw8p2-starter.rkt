;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw8p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)
; == Homework 8, Problem 2 ==

; We are now going to return to the project, designing several common functions for
; pre-processing text (which is to say, cleaning up raw text data for use in such
; operations as counting words) and applying sentiment analysis (which, in our case,
; will mean determining whether a text is positive, negative, or neutral, based on
; the words that it up).

; Throughout these problems you should make appropriate use of list abstractions
; and local/lambda.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Warmup :)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO #1: to begin, design the (very useful) abstraction item-in-list?
; that accepts a value, a list of elements (of the same type as the value),
; and an equality function (that takes two values of the same type as the
; supplied value/list and determines if they are equal), and outputs
; whether or not the value was found in the list via the equality function.
; Note: this is very similar to the member? function, which you should
; avoid since it uses equal? (which you have similarly been instructed
; to avoid.)

; THEN, use this abstraction to implement string-in-list? and
; number-in-list?, which specialize for (lists of) strings and numbers.

; For clarity, some tests have been provided below.



(check-expect
 (item-in-list? "a" (list "a" "bb" "ccc") string=?)
 #true)

(check-expect
 (item-in-list? 4 (list 1 2 3) =)
 #false)

(check-expect
 (string-in-list? "bb" (list "a" "bb" "ccc"))
 #true)

(check-expect
 (number-in-list? 3 (list 1 2 3))
 #true)

; item-in-list? : Any [List-of Any] [Any Any -> Boolean] -> Boolean
; checks if a given item is in a given list using a given equality function
(define (item-in-list? val lov f-equal?)
  (ormap (lambda (curr) (f-equal? curr val)) lov))

; string-in-list? : String [List-of String] -> Boolean
; checks if a given string is in a given list of strings
(define (string-in-list? s los)
  (item-in-list? s los string=?))

; number-in-list? : Real [List-of Real] -> Boolean
; checks if a given number is in a given list of numbers
(define (number-in-list? n lon)
  (item-in-list? n lon =))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Text Pre-Processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; As we proceed to analyze text, it's nice if we can perform some data
; "cleaning", which is to say, taking a word that that is in various cases
; and may include punctuation/digits (e.g., "WoRD1?") and make it easy for us
; to work with (e.g., "word").


(define PUNCTUATION "!\"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~’‘——–")
(define DIGITS "0123456789")

; TODO #2: given the strings above, design the function preprocess-word that
; removes any digits & punctuation, and also converts the word to all
; lowercase. The following functions are likely to be quite useful:
; - string-downcase: converts a string to lowercase
; - explode: given a string, produces a list of 1String's, which are
;            the characters that make up the word (a 1String is a known
;            data type that you don't have to design)
; - implode: given a list of 1String's, combine them to produce a single word
; - string-contains?: determines if one string is contained within another
; For clarity, some tests have been provided below.


(check-expect (preprocess-word "Foo-Bar") "foobar")
(check-expect (preprocess-word "She's") "shes")
(check-expect (preprocess-word "Number-1!") "number")

; preprocess-word : String -> String
; removes any digits and punctuation from the given string and converts all letters to lowercase
(define (preprocess-word word)
  (string-downcase
   (implode
    (filter (lambda (c)
              (not (or (string-contains? c PUNCTUATION) (string-contains? c DIGITS))))
            (explode word)))))


; TODO #3: design the function preprocess-words that given a list of input words
; and a list of "forbidden words" (that is, those we want to exclude from our
; analysis), return the words that are not forbidden, and not empty, already
; preprocessed as described above (i.e., lowercase without digits/punctuation).
; For clarity, some tests have been provided below.


; For more info on this idea: https://en.wikipedia.org/wiki/Stop_word
(define FORBIDDEN (list "uh" "um" "like"))


(check-expect (preprocess-words '() FORBIDDEN) '())
(check-expect (preprocess-words (list "hello" "" "world") FORBIDDEN)
              (list "hello" "world"))
(check-expect (preprocess-words (list "Uh" "um" "LIKE" "Hello," "" "World!?" "uh") FORBIDDEN)
              (list "hello" "world"))

; preprocess-words : [List-of String] [List-of String] -> [List-of String]
; removes words from the first given list that are in the second given list and all empty strings
(define (preprocess-words words forbs)
  (filter (lambda (n)
            (not (or (string=? n "") (string-in-list? n forbs)))) (map preprocess-word words)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simple Sentiment Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Let's pretend for a moment that we had a list of words that were considered positive
; and a list of words that were considered negative. A simple way of understanding
; whether a list of words (representing a pre-processed sentence) is positive, negative,
; or netural would be to count up how many words appear in the positive list and
; subtract how many appear in the negative list.

; For example, imagine we have the following positive/negative lists...

(define POSITIVE-WORDS (list "good" "happy" "awesome" "fun" "lovely"))
(define NEGATIVE-WORDS (list "sad" "bad" "grumpy" "boring" "fear" "rage"))

; then (list "what" "a" "lovely" "day" "i" "am" "happy" "and" "having" "fun")
; has 3 positive words (lovely, happy, fun) and 0 negative words, and so a score of
; 3 - 0 = 3 suggests it might be positive overall.


; TODO #4: design the function polarity-count that takes a list of words (representing
; a preprocessed sentence for analysis), as well as preprocessed lists of positive &
; negative words, and outputs the difference between the count of those words from the
; first (sentence) list that appear in the positive list and those that appear in the
; negative list. For clarity, some tests have been provided.


(check-expect (polarity-count
               (list "what" "a" "lovely" "day"
                     "i" "am" "happy" "and" "having" "fun")
               POSITIVE-WORDS NEGATIVE-WORDS)
              3)

(check-expect (polarity-count
               (list "what" "a" "grumpy" "boring" "movie"
                     "im" "happy" "to" "leave" "soon")
               POSITIVE-WORDS NEGATIVE-WORDS)
              -1)

; polarity-count : [List-of String] [List-of String] [List-of String] -> Integer
; returns difference between number of positive words in the given list and negative words
(define (polarity-count words positive negative)
  (local [; count-words : [List-of String] -> Natural
          ; counts the number of words that are in the given list
          (define (count-words check-words)
            (foldr + 0 (map (lambda (word) (if (string-in-list? word check-words) 1 0)) words)))]
    (- (count-words positive) (count-words negative))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Learning Word Polarity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Recall that in Homework 5 you were asked to design some data. In particular,
; you designed a numeric label for text...


; A Label is {-1.0, 1.0, 0.0}
; Interpretation: a text is negative, positive, or neutral

(define LABEL-NEG -1.0)
(define LABEL-POS 1.0)
(define LABEL-NEUT 0.0)


; As well as a structure in which to store information about numeric word
; polarity across a number of labeled texts...


(define-struct wpd [word sum count])

; A WordPolarityData (WPD) is a (make-wpd String Real Nat)
; - word: the word we are collecting information about
; - sum: the total of all the label data we have
; - count: the number of texts that contained this word
; Interpretation: information about labeled texts for a word

(define WPD-HAPPY (make-wpd "happy" 0.5 1))
(define WPD-DAY (make-wpd "day" 0.5 1))

(define WPD-THIS (make-wpd "this" 0.0 2))
(define WPD-TEXT (make-wpd "text" 0.0 2))
(define WPD-IS (make-wpd "is" 0.0 2))
(define WPD-GOOD (make-wpd "good" 1.25 2))
(define WPD-BAD (make-wpd "bad" -0.25 1))


; To practice working with this data, you should be able to calculate the polarity
; value of a particular word. For instance, "good" should be 0.625, since it appeared
; twice and had a sum of 1.25 (so 1.25 / 2 = 0.625).


; TODO #5: design polarity-avg that produces the polarity value given a word and
; a list of word polarity data. If the word is not found in the list, simply return
; 0.0 (neutral). For clarity, some tests have been provided.

; As a suggested approach:
; - Try to find the element (is there a list abstraction that could limit the list
;   to the one, if it exists, that matches the supplied word?)
; - If there is no match, 0.0; otherwise divide sum by count

(define LOWPD-1
  (list WPD-HAPPY WPD-DAY))

(define LOWPD-2
  (list WPD-THIS WPD-TEXT WPD-IS
        WPD-GOOD WPD-BAD))


(check-expect
 (polarity-avg "happy" LOWPD-1)
 0.5)

(check-expect
 (polarity-avg "good" LOWPD-2)
 0.625)

(check-expect
 (polarity-avg "terrible" LOWPD-2)
 0.0)

; polarity-avg : String [List-of WPD] -> Real
; returns the polarity value of a given word and given list of word polarity data
(define (polarity-avg word lowpd)
  (local [; A Limited is a [List-of WPD]
          ; Interpretation: contains only the given word in the given list of word polarity data
          (define LIMITED (filter (lambda (wpd) (string=? word (wpd-word wpd))) lowpd))]
    (if (= (length LIMITED) 0) 0.0
        (/ (wpd-sum (list-ref LIMITED 0))
           (wpd-count (list-ref LIMITED 0))))))

; Now it's time to actually time to produce this polarity data based upon
; labeled text :)

; To start, here's how you'll get the labeled sentences...


(define-struct lt [words label])

; A LabeledText is a (make-lt [List-of String] Label)
; Interpretation: a list of preprocessed words and their associated label

(define LT-HAPPY-DAY (make-lt (list "happy" "day") LABEL-POS))
(define LT-TEXT-GOOD (make-lt (list "this" "text" "is" "good") LABEL-POS))
(define LT-TEXT-BAD (make-lt (list "this" "text" "is" "bad") LABEL-NEG))
(define LT-GOOD (make-lt (list "good") LABEL-POS))


; So, now your task is to take a list of these labeled texts and produce a
; corresponding list of word polarity data. The basic idea is that every
; word in a text gets an equal share of the label for that text. So 1.0 gets
; split in half for LT-HAPPY-DAY, and so each gets 0.5 added to their sum
; and 1 added to their count. So, very similar to the item counting you did
; in Homework 6, you now need to go text-by-text, word-by-word, and add up
; each word's summed label share as well as the total number of times it
; occurs. (Note: this time around, you have list abstractions at your
; disposal, which should make some parts easier!)

; As a suggested approach (in case it helps):
; - for each labeled text, first to last, add that text's contents to the result
;   (hint: what list abstraction does this sound like?)
; - for a given labeled text, calculate the contribution each word will get
;   (hint: this should sound like you are storing an intermediate value ...);
;   then, for each word, left to right, add that word's contribution to the
;   result (hint: what list abstraction does this sound like?)
; - for each word you are adding to the result, you have to find it in the list,
;   which sounds a lot like HW6-P3 :)


; TODO #6: design word-polarities that accepts a list of labeled texts and produces
; a list of word polarity data. For clarity, some tests have been provided.


(check-expect
 (word-polarities (list LT-HAPPY-DAY))
 LOWPD-1)


(check-expect
 (word-polarities (list LT-TEXT-GOOD
                        LT-TEXT-BAD
                        LT-GOOD))
 LOWPD-2)

; word-polarities : [List-of LT] -> [List-of WPD]
; given a list of labeled texts, produce a corresponding list of word polarity data
(define (word-polarities lolt)
  (local [; indexof : String [List-of WPD] Natural -> Natural
          ; returns the index of a given word in a given list of word polarity data
          (define (indexof word lowpd i)
            (cond
              [(empty? lowpd) -1]
              [(cons? lowpd)
               (if (string=? word (wpd-word (first lowpd)))
                   i
                   (indexof word (rest lowpd) (add1 i)))]))
          ; update-wpd : [List-of WPD] Real Natural Natural -> [List-of WPD]
          ; updates the word polarity data at a given location in a given list of word polarity data
          (define (update-wpd lowpd polarity i curr)
            (cond
              [(empty? lowpd) '()]
              [(cons? lowpd)
               (if (= i curr) (cons (make-wpd (wpd-word (first lowpd))
                                              (+ (wpd-sum (first lowpd)) polarity)
                                              (add1 (wpd-count (first lowpd))))
                                    (update-wpd (rest lowpd) polarity i (add1 curr)))
                   (cons (first lowpd) (update-wpd (rest lowpd) polarity i (add1 curr))))]))
          ; sum-lolt : [List-of LT] [List-of WPD] -> [List-of WPD]
          ; iterates through a given list of labeled text and returns the text's word polarity data
          (define (sum-lolt lolt result)
            (cond
              [(empty? lolt) result]
              [(cons? lolt) (sum-lolt (rest lolt)
                                      (sum-lt (lt-words (first lolt))
                                              (/ (lt-label (first lolt))
                                                 (length (lt-words (first lolt)))) result))]))
          ; sum-lt : [List-of String] Real [List-of WPD] -> [List-of WPD]
          ; iterates through words in a labeled text and returns their word polarity data
          (define (sum-lt low polarity result)
            (cond
              [(empty? low) result]
              [(cons? low)
               (if (= (indexof (first low) result 0) -1)
                   (sum-lt (rest low) polarity
                           (append result (list (make-wpd (first low) polarity 1))))
                   (sum-lt (rest low) polarity
                           (update-wpd result polarity (indexof (first low) result 0) 0)))]))]
    (sum-lolt lolt '())))