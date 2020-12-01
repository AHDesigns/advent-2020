;;; day-1-test.el --- Tests for day 1 (advent of code 2020)

;;; Commentary:

;;; Code:

(load-file "./day-1.el")

(ert-deftest day-1-test--square-2 ()
  (should (= 4 (day-1-square 2))))

;;; day-1-test.el ends here
