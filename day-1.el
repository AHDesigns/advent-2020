;;; day-1.el --- Day 1 (advent of code 2020) -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'dash)
(setq lexical-binding t)

(defun day-1 (list)
  (-> list
      (my-string-to-list)
      (get-tuple-that-sums-to-2020)
      (get-product)))

(defun get-tuple-that-sums-to-2020 (list)
  "find first two values in LIST that sum to 2020 or return nil"
  (pcase list
    (`() ())
    (`(,head) ())
    (`(,head . ,tail)
     (-if-let (match-number (-find (-partial 'sum-2020? head) tail))
       (list head match-number)
       (get-tuple-that-sums-to-2020 tail)))))

(defun sum-2020? (n1 n2)
  "Do N1 and N2 sum to 2020."
  (message "testing %d %d" n1 n2) (= 2020 (+ n1 n2)))

(defun get-product (list)
  (when list 
    (-reduce '* list)))

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

(ert-deftest day-1-test--unhappy-paths ()
  (should (eql '() (day-1 "1 2 3")))
  (should (eql '() (day-1 "")))
  (should (eql '() (day-1 "not a number")))
  (should (eql '() (day-1 "1
2
3
4"))))

(day-1 (read-file "./inputs/day-1.txt"))
;; 1005459

;;; day-1.el ends here
