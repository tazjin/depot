;;; dottime.el --- use dottime in the modeline
;;
;; Copyright (C) 2019 Google Inc.
;;
;; Author: Vincent Ambo <tazjin@google.com>
;; Version: 1.0
;; Package-Requires: (cl-lib)
;;
;;; Commentary:
;;
;; This package changes the display of time in the modeline to use
;; dottime (see https://dotti.me/) instead of the standard time
;; display.
;;
;; Modeline dottime display is enabled by calling
;; `dottime-display-mode' and dottime can be used in Lisp code via
;; `dottime-format'.

(require 'cl-lib)
(require 'time)

(defun dottime--format-string ()
  "Creates the dottime format string for `format-time-string'
  based on the local timezone."

  (let* ((offset-sec (car (current-time-zone)))
         (offset-hours (/ offset-sec 60 60)))
    (if (/= offset-hours 0)
        (concat "%m-%dT%H·%M" (format "%0+3d" offset-hours))
      "%m-%dT%H·%M")))

(defun dottime--display-time-update-advice (orig)
  "Function used as advice to `display-time-update' with a
  rebound definition of `format-time-string' that renders all
  timestamps as dottime."

  (cl-letf* ((format-orig (symbol-function 'format-time-string))
             ((symbol-function 'format-time-string)
              (lambda (&rest _)
                (funcall format-orig (dottime--format-string) nil t))))
    (funcall orig)))

(defun dottime-format (&optional time)
  "Format the given TIME in dottime. If TIME is nil, the current
  time will be used."

  (format-time-string (dottime--format-string) time t))

(defun dottime-display-mode (arg)
  "Enable time display as dottime. Disables dottime if called
  with prefix 0 or nil."

  (interactive "p")
  (if (or (eq arg 0) (eq arg nil))
      (advice-remove 'display-time-update #'dottime--display-time-update-advice)
    (advice-add 'display-time-update :around #'dottime--display-time-update-advice))
  (display-time-update))

(provide 'dottime)
