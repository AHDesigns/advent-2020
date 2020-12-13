;;; day-2.el --- Day 2 (advent of code 2020) -*- lexical-binding: t -*-

(require 'dash)
(require 's)
(load (s-replace (buffer-name) "utils" (buffer-file-name)))

(defun day-2 (list)
  (-some--> list
    (my-string-to-list it)
    (get-valid-passwords it)))

(defun day-2-part2 (list)
  (-some--> list
    (my-string-to-list it)
    (get-valid-passwords-2 it)))

(defun get-valid-passwords (list)
  "count all valid passwords in LIST"
  (-some--> list
    (-filter 'valid-password? it)
    (-map (lambda (_) 1) it)
    (-sum it)))

(defun get-valid-passwords-2 (list)
  "count all valid passwords in LIST"
  (-some--> list
    (-filter 'valid-password-2? it)
    (-map (lambda (_) 1) it)
    (-sum it)))

(defun valid-password? (str)
  (save-match-data
    (and (string-match "\\([0-9]+\\)-\\([0-9]+\\) \\(.\\): \\(.*\\)" str)
	 (let ((min-count (string-to-number (match-string 1 str)))
	       (max-count (string-to-number (match-string 2 str)))
	       (char (match-string 3 str))
	       (password (match-string 4 str)))
	   (let ((count  (count-occurences-of char password)))
	     (<= min-count count max-count))))))

(defun valid-password-2? (str)
  (save-match-data
    (and (string-match "\\([0-9]+\\)-\\([0-9]+\\) \\(.\\): \\(.*\\)" str)
	 (let ((p1 (match-as-position 1 str))
	       (p2 (match-as-position 2 str))
	       (char (match-string 3 str))
	       (password (match-string 4 str)))
	   (xor (equal char (subchar password p1))
		(equal char (subchar password p2)))))))

(defun match-as-position (idx str)
  "get match number IDX from match-string and subtract 1 to 0 index it"
  (- (string-to-number (match-string idx str)) 1))

(defun subchar (string at)
  "get char AT index from STRING or nil if out of range"
  (when (< at (length string))
    (substring string at (+ at 1))))

(defun count-occurences-of (char str)
  "count occurences of CHAR in STR"
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (count-matches char)))

;;; tests
(ert-deftest day-2-test--given-example ()
  (should (eql 2 (day-2 "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"))))

(ert-deftest day-2--valid-password?-test ()
  (test-for 'valid-password?
	    '(("1-3 a: abcde" t)
	      ("1-1 a: bab" t)
	      ("10-11 a: aaaaaaaaaa" t)
	      ("1-3 b: baabaabcde" t)
	      ("4-5 b: ababababa" t)
	      ("1-3 b: baabaabcbde" nil)
	      ("1-3 b: a" nil)
	      ("" nil))))

(ert-deftest day-2--valid-password-2?-test ()
  (test-for 'valid-password-2?
	    '(("1-3 a: abcde" t)
	      ("2-4 a: babe" t)
	      ("2-4 a: bsba" t)
	      ("2-4 a: bab" t)
	      ("2-4 a: aaa" t)
	      ("1-1 a: bab" nil)
	      ("2-4 a: abab" nil)
	      ("2-4 a: babab" nil)
	      ("10-11 a: aaaaaaaaaab" t)
	      ("" nil))))

(message "%s" (day-2 (read-file "./inputs/day-2.txt")))
;; "454"

(message "%s" (day-2-part2 (read-file "./inputs/day-2.txt")))
;; "649"
