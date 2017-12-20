;; Copyright (C) 2016-2017  Vincent Ambo <mail@tazj.in>
;;
;; This file is part of Gemma.
;;
;; Gemma is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defpackage gemma
  (:use :cl
        :alexandria
        :hunchentoot
        :local-time
        :cl-json))
(in-package :gemma)

;; TODO: Store an average of how many days it was between task
;; completions. Some of the current numbers are just guesses
;; anyways.

;;
;; Define task management system
;;
(defclass task ()
  (;; (Unique) name of the task
   (name :type symbol
         :initarg :name
         :accessor name-of)

   ;; Maximum completion interval
   (days :type integer
         :initarg :days
         :accessor days-of)

   ;; Optional description
   (description :type string
                :initarg :description
                :accessor description-of)

   ;; Last completion time
   (done-at :type local-time:timestamp
            :accessor last-done-at)))

(defvar *tasks*
  (make-hash-table)
  "List of all tasks registered in this Gemma instance.")

(defmacro deftask (task-name days &optional description)
  `(setf (gethash (quote ,task-name) *tasks*)
         (make-instance (quote task)
                        :name (quote ,task-name)
                        :days ,days
                        :description (or ,description ""))))

(defun get-task (name)
  (gethash name *tasks*))

(defun list-tasks ()
  (alexandria:hash-table-values *tasks*))

(defun days-remaining (task)
  "Returns the number of days remaining before the supplied TASK reaches its
maximum interval."
  (let* ((expires-at (local-time:timestamp+ (last-done-at task)
                                            (days-of task) :day))
         (secs-until-expiry (local-time:timestamp-difference expires-at
                                                             (local-time:now))))
    (round (/ secs-until-expiry 60 60 24))))

(defun sort-tasks (tasks)
  "Sorts TASKS in descending order by number of days remaining."
  (sort (copy-list tasks)
        (lambda (t1 t2) (< (days-remaining t1)
                           (days-remaining t2)))))

(defun complete-task (name &optional at)
  "Mark the task with NAME as completed, either now or AT specified time."
  (setf (last-done-at (get-task name))
        (or at (local-time:now))))

;;
;; Define web API
;;

(defun response-for (task)
  "Create a response object to be JSON encoded for TASK."
  `((:name . ,(name-of task))
    (:description . ,(description-of task))
    (:remaining . ,(days-remaining task))))

(defun start-gemma ()
  ;; Set up web server
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))

  ;; ... and register all handlers.

  ;; Task listing handler
  (hunchentoot:define-easy-handler
   (get-tasks :uri "/tasks") ()

   (setf (hunchentoot:content-type*) "application/json")
   (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
   (json:encode-json-to-string
    ;; Construct a frontend-friendly representation of the tasks.
    (mapcar #'response-for (sort-tasks (list-tasks)))))

  ;; Task completion handler
  (hunchentoot:define-easy-handler
   (complete-task-handler :uri "/complete") (task)
   (setf (hunchentoot:content-type*) "application/json")
   (let* ((key (intern (json:camel-case-to-lisp task) "GEMMA")))
     (format t "Marking task ~A as completed" key)
     (complete-task key)
     (json:encode-json-to-string (response-for (get-task key))))))

;; (not-so) example tasks

;; Bathroom tasks
(deftask bathroom/wipe-mirror 7)
(deftask bathroom/wipe-counter 7)

;; Bedroom tasks
(deftask bedroom/change-sheets 7)
(deftask bedroom/vacuum 10)

;; Kitchen tasks
(deftask kitchen/normal-trash 3)
(deftask kitchen/green-trash 5)
(deftask kitchen/blue-trash 5)
(deftask kitchen/wipe-counters 3)
(deftask kitchen/vacuum 5 "Kitchen has more crumbs and such!")

;; Entire place
(deftask clean-windows 60)

;; Experimentation / testing stuff

(defun randomise-completion-times ()
  "Set some random completion timestamps for all tasks"
  (mapcar
   (lambda (key) (complete-task key (local-time:timestamp- (local-time:now)
                                                           (random 14)
                                                           :day)))
   (alexandria:hash-table-keys *tasks*)))

;; (randomise-completion-times)
