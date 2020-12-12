;;; day-1.el --- Day 1 (advent of code 2020) -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'dash)

(defun day-1 (list)
  (-some--> list
      (my-string-to-list it)
      (get-tuple-that-sums-to 2020 it)
      (-reduce '* it)))

(defun day-1-part2 (list)
  (-some--> list
    (my-string-to-list it)
    (get-three-tuple-that-sums-to 2020 it)

    (-reduce '* it)))

;;;; helpers

(defun get-three-tuple-that-sums-to (target list)
  "find first two values in LIST that sum to 2020 or return nil"
  (pcase list
    (`() ())
    (`(,h1) ())
    (`(,h1 . ,tail) (-if-let (match (get-tuple-that-sums-to target tail h1))
				(cons h1 match)
		      (get-three-tuple-that-sums-to target tail)))))

(defun get-tuple-that-sums-to (target list &optional seed)
  "find first two values in LIST that sum to 2020 or return nil"
  (unless seed (setq seed 0))
  (pcase list
    (`() ())
    (`(,head) ())
    (`(,head . ,tail)
     (-if-let (match (--find (equal target (+ seed head it)) tail))
	 (list head match)
       (get-tuple-that-sums-to target tail seed)))))

(defun sum-to (target n1 n2)
  "Do N1 and N2 sum to 2020."
  (= target (+ n1 n2)))

(defun my-string-to-list (list)
  "Convert LIST of newline seperated numebrs into list."
  (mapcar 'string-to-number (split-string list "\n")))

(defun read-file (file)
  "Return FILE contents as string"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;;; Tests;

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
4
2020"))))

(ert-deftest day-1-part2-test--given-example ()
  (should (= 241861950 (day-1-part2 "1721
979
366
299
675
1456"))))

(ert-deftest day-1-part-2-test--unhappy-paths ()
  (should (eql '() (day-1-part2 "1 2 3")))
  (should (eql '() (day-1-part2 "")))
  (should (eql '() (day-1-part2 "not a number")))
  (should (eql '() (day-1-part2 "1
2
3
4
2020
2019"))))

(message "%s" (day-1 (read-file "./inputs/day-1.txt")))
;; 1005459

;; obviously need to work on why I need this :P
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)
(message "%s" (day-1-part2 (read-file "./inputs/day-1.txt")))
;; 92643264

;;; day-1.el ends here
