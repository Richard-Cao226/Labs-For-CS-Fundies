;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw5p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Homework 5, Problem 2 ==

; You are to design a reminders program, which
; allows you to keep track of all the tasks in
; your busy digital life.

; In this program, a Group is an organizational unit
; that has a title (such as "Today") and a list of tasks.
; Each Task has it's own descriptive name (such as
; "Submit Homework 5") as well as an indication of whether
; the task has been completed or not.

; When you run reminders, you supply it a group; the
; program should then show you the group's title and
; the first task (including it's description and some
; way of indicating whether it's been completed or not);
; if the task list is empty, a friendly congratulations
; might be in order ;)
;
; Pressing the "n" key on the keyboard (for "next") should
; allow you to scroll through the tasks in order - it should
; cycle such that after seeing the last task, you come back to
; the first. When viewing a task, pressing space bar toggles
; whether the task is complete or not. When the program ends,
; it should return the number of tasks in the group that are
; incomplete.

; TODO #1: Finish designing the data you will need for this program.
; It will be useful (and fun!) to have a reasonable number of
; examples.

(define-struct task [desc done?])

; A Task is a (make-task String Boolean)
; Interpretation: the reminder task and whether it is complete

(define TASK-1 (make-task "Wash Dishes" #false))
(define TASK-2 (make-task "Do Homework" #true))
(define TASK-3 (make-task "Clean Room" #true))
(define TASK-4 (make-task "Eat Dinner" #false))
(define TASK-4-complete (make-task "Eat Dinner" #true))

(define (task-temp task)
  (... (task-desc task) ...
       (task-done? task) ...))


; A ListOfTasks (LoT) is one of:
; - '()
; - (cons Task LoT)
; Interpretation: a list of tasks

(define LOT-0 '())
(define LOT-1 (cons TASK-1 LOT-0))
(define LOT-2 (cons TASK-2 LOT-1))
(define LOT-3 (cons TASK-3 LOT-2))
(define LOT-4 (cons TASK-4 LOT-3))
(define LOT-4-complete (cons TASK-4-complete LOT-3))
(define LOT-next-1 (cons TASK-4 LOT-0))
(define LOT-next-2 (cons TASK-1 LOT-next-1))
(define LOT-next-3 (cons TASK-2 LOT-next-2))
(define LOT-next-4 (cons TASK-3 LOT-next-3))

(define (lot-temp lot)
  (...
   (cond
     [(empty? lot) ...]
     [(cons? lot) ...
      (task-temp (first lot)) ...
      (lot-temp (rest lot)) ...])))


(define-struct group [title tasks])

; A Group is a (make-group String LoT)
; Interpretation: a titled task list

(define GROUP-empty (make-group "Today" LOT-0))
(define GROUP-1 (make-group "Today" LOT-4))
(define GROUP-next (make-group "Today" LOT-next-4))
(define GROUP-complete (make-group "Today" LOT-4-complete))

(define (group-temp group)
  (... (group-title group) ...
       (lot-temp (group-tasks group)) ...))


; TODO #2: Finish designing the World program reminders.
; You are welcome to be creative as to how the program
; visualizes the group, the task, and completion status.
; Hint: for each function we started, follow the template
; closely to determine any further helper(s).


; reminders : Group -> NatNumber
; Visualizes an interactive reminders list
; and returns the number of remaining incomplete
; tasks at exit
(define (reminders initial-group)
  (num-not-complete
   (big-bang initial-group
     [to-draw draw-group]
     [on-key key-group])))

(check-expect (num-not-complete-lot LOT-0) 0)
(check-expect (num-not-complete-lot LOT-4) 2)

; num-not-complete-lot : LOT -> Natural
; counts the number of incomplete tasks in a given list of tasks
(define (num-not-complete-lot lot)
  (cond
    [(empty? lot) 0]
    [(cons? lot) (+ (complete? (first lot)) (num-not-complete-lot (rest lot)))]))

(check-expect (complete? TASK-1) 1)
(check-expect (complete? TASK-2) 0)

; complete? : Task -> Natural
; checks if given task is complete
(define (complete? task)
  (cond
    [(task-done? task) 0]
    [else 1]))
  
(check-expect (num-not-complete GROUP-empty) 0)
(check-expect (num-not-complete GROUP-1) 2)

; num-not-complete : Group -> NatNumber
; counts the number of incomplete tasks in a given group
; in the group
(define (num-not-complete group)
  (num-not-complete-lot (group-tasks group)))

(check-expect (no-or-yes-tasks "Today" LOT-0)
              (overlay (text (string-append "Today - No Tasks!") 40 "blue")
                       (rectangle 700 100 "outline" "black")))
(check-expect (no-or-yes-tasks "Today" LOT-1)
              (overlay (above (text "Today" 40 "blue") (text "Incomplete - Wash Dishes" 30 "red"))
                       (rectangle 700 100 "outline" "black")))

; no-or-yes-tasks : String LOT -> Image
; visualizes given list of tasks after checking if it is empty
(define (no-or-yes-tasks title lot)
  (cond
    [(empty? lot)
     (overlay (text (string-append title " - No Tasks!") 40 "blue")
              (rectangle 700 100 "outline" "black"))]
    [(cons? lot)
     (show-task title (first lot) (= (num-not-complete-lot lot) 0))]))

(check-expect (show-task "Today" TASK-2 #t)
              (overlay (above (text "Today - All Tasks Complete!" 40 "blue")
                              (text "Completed - Do Homework" 30 "red"))
                       (rectangle 700 100 "outline" "black")))
(check-expect (show-task "Today" TASK-1 #f)
              (overlay (above (text "Today" 40 "blue") (text "Incomplete - Wash Dishes" 30 "red"))
                       (rectangle 700 100 "outline" "black")))
(check-expect (show-task "Today" TASK-2 #f)
              (overlay (above (text "Today" 40 "blue") (text "Completed - Do Homework" 30 "red"))
                       (rectangle 700 100 "outline" "black")))

; show-task : String Task -> Image
; visualizes given task
(define (show-task title task all-complete?)
  (cond
    [all-complete? (overlay (above (text (string-append title " - All Tasks Complete!") 40 "blue")
                                   (text (string-append "Completed - " (task-desc task)) 30 "red"))
                            (rectangle 700 100 "outline" "black"))]
    [(task-done? task)
     (overlay (above (text title 40 "blue")
                     (text (string-append "Completed - " (task-desc task)) 30 "red"))
              (rectangle 700 100 "outline" "black"))]
    [else (overlay (above (text title 40 "blue")
                          (text (string-append "Incomplete - " (task-desc task)) 30 "red"))
                   (rectangle 700 100 "outline" "black"))]))

(check-expect (draw-group GROUP-empty)
              (overlay (text (string-append "Today - No Tasks!") 40 "blue")
                       (rectangle 700 100 "outline" "black")))
(check-expect (draw-group GROUP-1) (overlay (above (text "Today" 40 "blue")
                                                   (text "Incomplete - Eat Dinner" 30 "red"))
                                            (rectangle 700 100 "outline" "black")))

; draw-group : Group -> Image
; visualizes the reminder group
(define (draw-group group)
  (no-or-yes-tasks (group-title group) (group-tasks group)))

(check-expect (key-group GROUP-1 "n") GROUP-next)
(check-expect (key-group GROUP-1 " ") GROUP-complete)
(check-expect (key-group GROUP-1 "s") GROUP-1)

; key-group : Group KeyEvent -> Group
; when "n" is pressed, rotate's the group's
; task list (first goes on the end);
; when " " is pressed, flips the completion
; status of the current task
(define (key-group group ke)
  (cond
    [(key=? ke "n") (make-group (group-title group)
                                (rest (append (group-tasks group)
                                              (list (list-ref (group-tasks group) 0)))))]
    [(key=? ke " ") (make-group (group-title group)
                                (cons (make-task
                                       (task-desc (first (group-tasks group)))
                                       (not (task-done? (first (group-tasks group)))))
                                      (rest (group-tasks group))))]
    [else group]))


(reminders GROUP-1)