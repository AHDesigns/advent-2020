;;; utils.el --- (advent of code 2020) -*- lexical-binding: t -*-

;;; Commentary:
;; utility functions for use throughout daily tasks

;;; Code:
(defun read-file (file)
  "Return FILE contents as string"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun my-string-to-number-list (list)
  "Convert LIST of newline seperated numebrs into list."
  (mapcar 'string-to-number (split-string list "\n")))

(defun my-string-to-list (str)
  (split-string str "\n"))

(defun test-for (fn tests)
  "test each cons pair in TESTS where each cell is (input output)"
  (dolist (test tests)
    (pcase test
      (`(,input ,output) (should (eql output (funcall fn input)))))))

;; obviously need to work on why I need this :P
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)
