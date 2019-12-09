;; -*- lexical-binding: t; -*-
;; Advent of Code 2019 - Day 2
(require 'dash)
(require 'ht)

(defvar day2/input
  [1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 1 9 19 1 19 5 23 1 13 23 27 1 27 6 31
     2 31 6 35 2 6 35 39 1 39 5 43 1 13 43 47 1 6 47 51 2 13 51 55 1 10 55
     59 1 59 5 63 1 10 63 67 1 67 5 71 1 71 10 75 1 9 75 79 2 13 79 83 1 9
     83 87 2 87 13 91 1 10 91 95 1 95 9 99 1 13 99 103 2 103 13 107 1 107 10
     111 2 10 111 115 1 115 9 119 2 119 6 123 1 5 123 127 1 5 127 131 1 10
     131 135 1 135 6 139 1 10 139 143 1 143 6 147 2 147 13 151 1 5 151 155 1
     155 5 159 1 159 2 163 1 163 9 0 99 2 14 0 0])

;; Puzzle 1

(defun day2/single-op (f state idx)
  (let* ((a (aref state (aref state (+ 1 idx))))
         (b (aref state (aref state (+ 2 idx))))
         (p (aref state (+ 3 idx)))
         (result (funcall f a b)))
    (aset state p (funcall f a b))))

(defun day2/operate (state idx)
  (pcase (aref state idx)
    (99 (aref state 0))
    (1 (day2/single-op #'+ state idx)
       (day2/operate state (+ 4 idx)))
    (2 (day2/single-op #'* state idx)
       (day2/operate state (+ 4 idx)))
    (other (error "Unknown opcode: %s" other))))

(defun day2/program-with-inputs (noun verb)
  (let* ((input (copy-tree day2/input t)))
    (aset input 1 noun)
    (aset input 2 verb)
    (day2/operate input 0)))

(message "Solution to day2/1: %s" (day2/program-with-inputs 12 2))

;; Puzzle 2
(let* ((used (ht))
       (noun 0)
       (verb 0)
       (result (day2/program-with-inputs noun verb)))
  (while (/= 19690720 result)
    (setq noun (random 100))
    (setq verb (random 100))
    (unless (ht-get used (format "%d%d" noun verb))
      (ht-set used (format "%d%d" noun verb) t)
      (setq result (day2/program-with-inputs noun verb))))

  (message "Solution to day2/2: %s%s" noun verb))
