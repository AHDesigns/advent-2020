;;; day-1.el --- Day 1 (advent of code 2020)

;;; Commentary:

;;; Code:

(require 'dash)
(setq lexical-binding t)

(defun day-1 (list)
  (-> list
      (my-string-to-list)
      (get-sum-2020-from-list)
      (get-product)))

(defun get-sum-2020-from-list (list)
  "finds the two numbers in a list that sum to 2020"
  (setq match nil)
  (dolist (elm list match)
    (dolist (inner-elm list)
      (unless (= elm inner-elm)
	(if (equal (+ elm inner-elm) 2020)
	    (setq match (list elm inner-elm)))))))

(defun sum-2020 (n1 n2)
  "Do N1 and N2 sum to 2020."
  (= 2020 (+ n1 n2)))

(defun get-product (list)
  (-reduce '* list))

(defun my-string-to-list (list)
  "Convert LIST of newline seperated numebrs into list."
  (mapcar 'string-to-number (split-string list "\n")))

(defun read-file (file)
  "Return FILE contents as string"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;; Tests;

(ert-deftest day-1-test--given-example ()
  (should (= 514579 (day-1 "1721
979
366
299
675
1456"))))

(day-1 (read-file "./inputs/day-1.txt"))
;; 1005459

;;; day-1.el ends here
