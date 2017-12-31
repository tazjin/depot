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
        :local-time
        :cl-json)
  (:import-from :sb-posix :getenv)
  (:shadowing-import-from :sb-posix :getcwd)
  (:export :start-gemma :config :entrypoint))
(in-package :gemma)

;; TODO: Store an average of how many days it was between task
;; completions. Some of the current numbers are just guesses
;; anyways.

(defmacro in-case-of (x &body body)
  "Evaluate BODY if X is non-nil, binding the value of X to IT."
  `(let ((it ,x))
     (when it ,@body)))

;; Set default configuration parameters
(defvar *gemma-port* 4242
  "Port on which the Gemma web server listens.")

(defvar *static-file-location*
  (or (in-case-of (sb-posix:getenv "out")
        (concatenate 'string it "/share/gemma/"))
      "frontend/")
  "Folder from which to serve static assets. If built inside of Nix,
  the folder is concatenated with the output path at which the files
  are expected to be.")

(defun initialise-persistence (data-dir)
  (defvar *p-tasks*
    (cl-prevalence:make-prevalence-system data-dir)
    "All tasks registered in this Gemma instance.")

  ;; Initialise database ID counter
  (or (> (length (cl-prevalence:find-all-objects *p-tasks* 'task)) 0)
      (cl-prevalence:tx-create-id-counter *p-tasks*)))

(defun config (&key port data-dir)
  "Configuration function for use in the Gemma configuration file."

  (in-package :gemma)
  (in-case-of port (defparameter *gemma-port* it))
  (initialise-persistence (or data-dir "data/")))

;;
;; Define task management system
;;

(defclass task ()
  ((id :reader id
       :initarg :id)

   ;; (Unique) name of the task
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
   (done-at :type timestamp
            :initarg :done-at
            :accessor last-done-at)))

(defmacro deftask (task-name days &optional description)
  (unless (get-task task-name)
    `(progn (cl-prevalence:tx-create-object
             *p-tasks*
             'task
             (quote ((name ,task-name)
                     (days ,days)
                     (description ,(or description ""))
                     (done-at ,(now)))))
            (cl-prevalence:snapshot *p-tasks*))))

(defun get-task (name)
  (cl-prevalence:find-object-with-slot *p-tasks* 'task 'name name))

(defun list-tasks ()
  (cl-prevalence:find-all-objects *p-tasks* 'task))

(defun days-remaining (task)
  "Returns the number of days remaining before the supplied TASK reaches its
maximum interval."
  (let* ((expires-at (timestamp+ (last-done-at task)
                                 (days-of task) :day))
         (secs-until-expiry (timestamp-difference expires-at (now))))
    (round (/ secs-until-expiry 60 60 24))))

(defun sort-tasks (tasks)
  "Sorts TASKS in descending order by number of days remaining."
  (sort (copy-list tasks)
        (lambda (t1 t2) (< (days-remaining t1)
                           (days-remaining t2)))))

(defun complete-task (name &optional at)
  "Mark the task with NAME as completed, either now or AT specified time."
  (cl-prevalence:tx-change-object-slots *p-tasks* 'task
                                        (id (get-task name))
                                        `((done-at ,(or at (now)))))
  (cl-prevalence:snapshot *p-tasks*))

;;
;; Define web API
;;

(defun response-for (task)
  "Create a response object to be JSON encoded for TASK."
  `((:name . ,(name-of task))
    (:description . ,(description-of task))
    (:remaining . ,(days-remaining task))))

(defun start-gemma ()
  (in-package :gemma)

  ;; Load configuration
  (load (pathname (or (getenv "GEMMA_CONFIG")
                      "/etc/gemma/config.lisp")))

  ;; Set up web server
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor
                  :port *gemma-port*
                  :document-root *static-file-location*))

  ;; Task listing handler
  (hunchentoot:define-easy-handler
   (get-tasks :uri "/tasks") ()

   (setf (hunchentoot:content-type*) "application/json")
   (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
   (encode-json-to-string
    ;; Construct a frontend-friendly representation of the tasks.
    (mapcar #'response-for (sort-tasks (list-tasks)))))

  ;; Task completion handler
  (hunchentoot:define-easy-handler
   (complete-task-handler :uri "/complete") (task)
   (setf (hunchentoot:content-type*) "application/json")
   (let* ((key (find-symbol (camel-case-to-lisp task) "GEMMA")))
     (format t "Marking task ~A as completed" key)
     (complete-task key)
     (encode-json-to-string (response-for (get-task key))))))

(defun entrypoint ()
  "This function serves as the entrypoint for ASDF-built executables.
  It joins the Hunchentoot server thread to keep the process running
  for as long as the server is alive."

  (start-gemma)
  (sb-thread:join-thread
   (find-if (lambda (th)
              (string= (sb-thread:thread-name th)
                       (format nil "hunchentoot-listener-*:~A" *gemma-port*)))
            (sb-thread:list-all-threads))))

;; Experimentation / testing stuff

(defun randomise-completion-times ()
  "Set some random completion timestamps for all tasks"
  (mapcar
   (lambda (task)
     (complete-task (name-of task)
                    (timestamp- (now)
                                (random 14)
                                :day)))
   (cl-prevalence:find-all-objects *p-tasks* 'task)))

(defun clear-all-tasks ()
  (mapcar (lambda (task) (cl-prevalence:tx-delete-object *p-tasks* 'task (id task)))
          (cl-prevalence:find-all-objects *p-tasks* 'task)))

;; (randomise-completion-times)
