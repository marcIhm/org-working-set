;;; owst.el --- Regression Tests for org-working-set.el

;; Copyright (C) 2011-2018 Free Software Foundation, Inc.

;; Author: Marc Ihm <1@2484.de>
;; Keywords: outlines, regression-tests, elisp
;; Requires: org, org-working-set
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Purpose:
;;
;;  Regression tests for package org-working-set.el.
;;
;; Setup:
;;
;;  None required
;;
;;
;;
;; Further reading:
;;
;;  See org-working-set.el, which is tested by this package
;;

;;; Code:

(require 'org-working-set)
(require 'cl-lib)
(require 'ert)

(defvar owst-work-buffer nil)
(defvar owst-ert-work-file (concat temporary-file-directory "owst-ert-work.org"))

;;
;; All tests
;;

(ert-deftest owst-test-aaa-test-test-setup ()
  (owst-with-test-setup
    (message "Testing test setup")))


(ert-deftest owst-test-clock-into-working-set ()
  (owst-with-test-setup
    (unwind-protect
	(progn
	  (let ((oidx--after-ws-delay 1) (org-index-clock-into-working-set nil))
	    (should (not (org-clock-is-active)))
	    (owst-do "o c c u r <return> z w e i <down> <return>")
	    (owst-do "w o r k i n g - s e t <return> s")
	    (sleep-for 2)
	    (should (not (org-clock-is-active)))
	    
	    (owst-do "w o r k i n g - s e t <return> w")
            (execute-kbd-macro (kbd "<S-return>"))
	    (sleep-for 2)
	    (should (not (org-clock-is-active)))
	    
	    (setq org-index-clock-into-working-set t)
	    (should (not (org-clock-is-active)))
	    (owst-do "o c c u r <return> z w e i <down> <return>")
	    (owst-do "w o r k i n g - s e t <return> s")
	    (sleep-for 2)
	    (should (org-clock-is-active))))
      (org-clock-out))))


(ert-deftest owst-test-working-set ()
  (owst-with-test-setup
    (owst-do "o c c u r <return> z w e i <down> <return>")
    (owst-do "w o r k i n g - s e t <return> s")
    (beginning-of-buffer)
    (owst-do "w o r k i n g - s e t <return>" "C-u")
    (should (looking-at ".* --8--"))))


(ert-deftest owst-test-assistant ()
  (owst-with-test-setup
    (should nil)))


(ert-deftest owst-test-working-set-do-not-clock ()
  (owst-with-test-setup
    (should (not (org-clock-is-active)))
    (owst-do "o c c u r <return> z w e i <down> <return>")
    (should (not oidx--ws-ids-do-not-clock))
    (owst-do "w o r k i n g - s e t <return> S")
    (should oidx--ws-ids-do-not-clock)
    (should (not (org-clock-is-active)))))


(ert-deftest owst-test-working-set-restore ()
  (owst-with-test-setup
    (owst-do "o c c u r <return> z w e i <down> <return>")
    (owst-do "w o r k i n g - s e t <return> s")
    (should (= (length oidx--ws-ids) 1))
    (owst-do "w o r k i n g - s e t <return> d")
    (should (= (length oidx--ws-ids) 0))
    (owst-do "w o r k i n g - s e t <return> u")
    (should (= (length oidx--ws-ids) 1))))


(ert-deftest owst-test-working-set-bottom-head ()
  (owst-with-test-setup
    (owst-do "o c c u r <return> - - 4 - - <return>")
    (owst-do "w o r k i n g - s e t <return> s")
    (beginning-of-buffer)
    (owst-do "w o r k i n g - s e t <return> b" "C-u")
    (forward-line)
    (should (looking-at ".* --2--"))
    (forward-line -1)
    (owst-do "w o r k i n g - s e t <return> h" "C-u")
    (should (looking-at ".* --4--"))))


(ert-deftest owst-test-working-set-menu-goto ()
  (owst-with-test-setup
    (owst-do "o c c u r <return> z w e i <down> <return>")
    (owst-do "w o r k i n g - s e t <return> s")
    (owst-do "o c c u r <return> e i n s <down> <return>")
    (owst-do "w o r k i n g - s e t <return> a")
    (owst-do "w o r k i n g - s e t <return> m <down> <return>")
    (should (looking-at ".* --8--"))))


(ert-deftest owst-test-working-set-menu-delete ()
  (owst-with-test-setup
    (owst-do "o c c u r <return> z w e i <down> <return>")
    (owst-do "w o r k i n g - s e t <return> s")
    (owst-do "o c c u r <return> e i n s <down> <return>")
    (owst-do "w o r k i n g - s e t <return> a")
    (should (= (length oidx--ws-ids) 2))
    (owst-do "w o r k i n g - s e t <return> m <down> d q")
    (should (= (length oidx--ws-ids) 1))))


(ert-deftest owst-test-working-set-menu-toggle-clocking ()
  (owst-with-test-setup
    (owst-do "o c c u r <return> z w e i <down> <return>")
    (owst-do "w o r k i n g - s e t <return> s")
    (owst-do "w o r k i n g - s e t <return> m ~")
    (should (looking-at "\\*~"))
    (execute-kbd-macro (kbd "c"))
    (should (looking-at "\\* "))))


(ert-deftest owst-test-double-working-set ()
  (owst-with-test-setup
    (owst-do "o c c u r <return> z w e i <down> <return>")
    (owst-do "w o r k i n g - s e t <return> s")
    (owst-do "o c c u r <return> e i n s <down> <return>")
    (owst-do "w o r k i n g - s e t <return> a")
    (owst-do "w o r k i n g - s e t <return> SPC SPC")
    (should (looking-at ".* --8--"))
    (owst-do "w o r k i n g - s e t <return> SPC")
    (should (looking-at ".* --8--"))
    (owst-do "w o r k i n g - s e t <return> SPC SPC")
    (should (looking-at ".* --13--"))))


(ert-deftest owst-test-nested-working-set ()
  (owst-with-test-setup
    (owst-do "o c c u r <return> v i e r <down> <return>")
    (owst-do "w o r k i n g - s e t <return> s")
    (search-forward "neun")
    (org-reveal)
    (owst-do "w o r k i n g - s e t <return> a")
    (should (= (length oidx--ws-ids) 1))))


;;
;; Helper functions
;;

(defmacro owst-with-test-setup (&rest body)
  "Execute body within test setup"
  (declare (indent 0) (debug t))
  `(progn
     (owst-setup-test)
     (unwind-protect
         (progn ,@body)
       (owst-teardown-test))))


(defun owst-do (keys &optional prefix)
  (execute-kbd-macro (kbd (concat prefix (if prefix " " "") "M-x o r g - w o r k i n g - s e t <return> " keys))))


(defun owst-setup-test ()
  (interactive)
  ;; remove any left over buffers
  (owst-remove-work-buffers)
  ;; create them new
  (owst-create-work-buffer)
  (switch-to-buffer owst-work-buffer)
  (basic-save-buffer)
  (org-agenda-file-to-front oidxt-ert-work-file)
  (owst-create-work-buffer)
  (switch-to-buffer owst-work-buffer)
  (org-cycle '(64))
  (delete-other-windows)
  (end-of-buffer))


(defun owst-teardown-test ()
  (interactive)
  (with-current-buffer owst-work-buffer (set-buffer-modified-p nil))
  (org-remove-file owst-ert-work-file))


(defun owst-remove-work-buffers ()
  "Remove any left over work buffers"
  (let ((b (get-buffer "owst-ert-work.org")))
    (when b
      (with-current-buffer b
	(set-buffer-modified-p nil))
      (kill-buffer b)))
  (setq owst-work-buffer nil))



;;
;; Test data
;;

(defun owst-create-work-buffer ()
  (unless owst-work-buffer
    (setq owst-work-buffer (find-file-noselect owst-ert-work-file)))
  (with-current-buffer owst-work-buffer
    (setq buffer-save-without-query t)
    (auto-save-mode t)
    (if (file-exists-p buffer-auto-save-file-name)
        (delete-file buffer-auto-save-file-name))
    (erase-buffer)
    (insert "
* eins
  :PROPERTIES:
  :ID:       588bda71-38b7-41a9-90f0-cc9fb39991fa
  :END:
* zwei
  :PROPERTIES:
  :ID:       5a16c863-1f7e-4636-9c47-74e4d49f72df
  :END:
* drei
  :PROPERTIES:
  :ID:       12ae411f-bdd4-4c92-9e24-75cf7858f586
  :END:
* vier
  :PROPERTIES:
  :ID:       caac71f6-74fa-4b6a-b732-66c9ceb0c483
  :END:
")
    (org-mode)
    (setq org-working-set-id "588bda71-38b7-41a9-90f0-cc9fb39991fa")
    owst-work-buffer))


(provide 'owst)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; owst.el ends here
