;; -*- lexical-binding: t; -*-
;; Advent of Code 2019 - Day 4

(require 'cl-lib)
(require 'dash)

;; Puzzle 1

(defun day4/to-digits (num)
  "Convert NUM to a list of its digits."
  (cl-labels ((steps (n digits)
                     (if (= n 0) digits
                       (steps (/ n 10) (cons (% n 10) digits)))))
    (steps num '())))

(defvar day4/input (-map #'day4/to-digits (number-sequence 128392 643281)))

(defun day4/filter-password (digits)
  "Determines whether the given rules match the supplied
  number."

  (and
   ;; It is a six digit number
   (= 6 (length digits))

   ;; Value is within the range given in puzzle input
   ;; (noop because the range is generated from the input)

   ;; Two adjacent digits are the same (like 22 in 122345).
   (car (-reduce-from (-lambda ((acc . prev) next)
                        (cons (or acc (= prev next)) next))
                      '(nil . 0) digits))

   ;; Going from left to right, the digits never decrease; they only
   ;; ever increase or stay the same (like 111123 or 135679).
   (car (-reduce-from (-lambda ((acc . prev) next)
                        (cons (and acc (>= next prev)) next))
                      '(t . 0) digits))))

;; Puzzle 2
;;
;; Additional criteria: If there's matching digits, they're not in a group.

(cl-defstruct day4/acc state prev count)

(defun day4/filter-longer-groups (digits)
  (let ((res (-reduce-from
              (lambda (acc next)
                (cond ;; sequence is broken and count was at 1 ->
                 ;; match!
                 ((and (= (day4/acc-count acc) 2)
                       (/= (day4/acc-prev acc) next))
                  (setf (day4/acc-state acc) t))

                 ;; sequence continues, counter increment!
                 ((= (day4/acc-prev acc) next)
                  (setf (day4/acc-count acc) (+ 1 (day4/acc-count acc))))

                 ;; sequence broken, reset counter
                 ((/= (day4/acc-prev acc) next)
                  (setf (day4/acc-count acc) 1)))

                (setf (day4/acc-prev acc) next)
                acc)
              (make-day4/acc :prev 0 :count 0) digits)))
    (or (day4/acc-state res)
        (= 2 (day4/acc-count res)))))

(let* ((simple (-filter #'day4/filter-password day4/input))
       (complex (-filter #'day4/filter-longer-groups simple)))
  (message "Solution to day4/1: %d" (length simple))
  (message "Solution to day4/2: %d" (length complex)))

