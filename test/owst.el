;;; owst.el --- Regression Tests for org-working-set.el

;; Copyright (C) 2019-2021 Free Software Foundation, Inc.

;; Author: Marc Ihm <1@2484.de>
;; Keywords: outlines, regression-tests, elisp
;; Requires: org, org-working-set
;; Version: 0.0.2

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
(require 'org-id)
(require 'cl-lib)
(require 'ert)

(defvar owst-ert-work-file (concat temporary-file-directory "owst-ert-work.org"))
(defvar owst-work-buffer nil)

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
	  (let ((org-working-set-clock-in nil))
	    (should (not (org-clock-is-active)))
	    (owst-goto "eins")
	    (owst-do "s <down>")
	    (sleep-for 1)
	    (should (not (org-clock-is-active)))
	    
	    (setq org-working-set-clock-in t)
	    (owst-goto "zwei")
	    (owst-do "s <down>")
	    (sleep-for 1)
	    (should (org-clock-is-active))))
      (org-clock-out))))


(ert-deftest owst-test-assistant ()
  (owst-with-test-setup
    (setq org-working-set-id nil)
    (owst-do "y e s <return> a")
    (should org-working-set-id)
    (should (string= org-working-set-id (car org-working-set--ids)))))


(ert-deftest owst-test-working-set-restore ()
  (owst-with-test-setup
    (owst-goto "zwei")
    (owst-do "s")
    (should (= (length org-working-set--ids) 1))
    (owst-do "d")
    (should (= (length org-working-set--ids) 0))
    (owst-do "u")
    (should (= (length org-working-set--ids) 1))))


(ert-deftest owst-test-working-set-goto-end ()
  (owst-with-test-setup
    (owst-goto "drei")
    (owst-do "s")
    (beginning-of-buffer)
    (owst-do "TAB l")
    (forward-line)
    (should (looking-at ".* vier"))))


(ert-deftest owst-test-working-set-return-after-quit ()
  (owst-with-test-setup
    (owst-goto "zwei")
    (owst-do "s")
    (owst-goto "drei")
    (owst-do "TAB q")
    (should (looking-at ".* drei"))))


(ert-deftest owst-test-working-set-add-and-find-inline ()
  (owst-with-test-setup
    (goto-char 0)
    (search-forward "Inline")
    (owst-do "a")
    (goto-char 0)
    (owst-do "SPC <return>")
    (should (looking-at "\*+ Inline"))))


(ert-deftest owst-test-working-set-menu-goto ()
  (owst-with-test-setup
    (owst-goto "zwei")
    (owst-do "s")
    (owst-goto "eins")
    (owst-do "a")
    (owst-do "SPC <down> <return>")
    (should (looking-at ".* zwei"))))


(ert-deftest owst-test-working-set-menu-delete ()
  (owst-with-test-setup
    (owst-goto "zwei")
    (owst-do "s")
    (owst-goto "eins")
    (owst-do "a")
    (should (= (length org-working-set--ids) 2))
    (owst-do "SPC d q")
    (should (= (length org-working-set--ids) 1))))


(ert-deftest owst-test-double-working-set ()
  (owst-with-test-setup
    (owst-goto "zwei")
    (owst-do "s")
    (owst-goto "eins")
    (owst-do "a")
    (owst-do "TAB TAB")
    (should (looking-at ".* zwei"))
    (owst-do "TAB")
    (should (looking-at ".* zwei"))
    (owst-do "TAB TAB")
    (should (looking-at ".* eins"))))


(ert-deftest owst-test-nested-working-set ()
  (owst-with-test-setup
    (owst-goto "drei")
    (owst-do "s")
    (owst-goto "vier")
    (owst-do "a")
    (should (= (length org-working-set--ids) 1))))


(ert-deftest owst-test-log-of-working-set ()
  (owst-with-test-setup
    (owst-goto "zwei")
    (owst-do "a")
    (owst-goto "eins")
    (org-end-of-meta-data t)
    (should (looking-at "[[:blank:]]+-"))))


(ert-deftest owst-test-when-node-has-gone ()
  (owst-with-test-setup
    (owst-goto "zwei")
    (owst-do "a")
    (org-delete-property "ID")
    (ignore-errors
      (owst-do "SPC d"))
    (should (= (length org-working-set--ids) 0))))


(ert-deftest owst-test-advice-for-problems ()
  (owst-with-test-setup
    (owst-goto "vier")
    (owst-do "a")
    (org-delete-property "ID")
    (ignore-errors
      (owst-do "SPC o"))
    (with-current-buffer "*Occur*"
      (should (looking-at "2 matches")))))


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
  (message (format "Executing test %S" (ert-test-name (ert--stats-current-test ert--current-run-stats))))
  ;; remove any left over buffers
  (owst-remove-work-buffers)
  ;; create them new
  (owst-create-work-buffer)
  (switch-to-buffer owst-work-buffer)
  (basic-save-buffer)
  (org-agenda-file-to-front owst-ert-work-file)
  (org-cycle '(64))
  (delete-other-windows)
  (end-of-buffer)
  (org-id-update-id-locations (list owst-ert-work-file))
  (ignore-errors
    (kill-buffer org-working-set--menu-buffer-name))
  (setq org-working-set--ids nil)
  (setq org-working-set--ids-do-not-clock nil)
  (setq org-working-set--id-not-found nil))


(defun owst-teardown-test ()
  (interactive)
  (with-current-buffer owst-work-buffer
    (set-buffer-modified-p nil)
    (basic-save-buffer))
  (org-remove-file owst-ert-work-file))


(defun owst-remove-work-buffers ()
  "Remove any left over work buffers"
  (let ((b (get-buffer "owst-ert-work.org")))
    (when b
      (with-current-buffer b
	(set-buffer-modified-p nil))
      (kill-buffer b)))
  (setq owst-work-buffer nil))


(defun owst-goto (name)
  (org-id-goto (cdr (assoc name owst-names-ids))))

;;
;; Test data
;;


(defvar owst-names-ids
  (list (cons "eins" "53e15dce-6f28-4674-bd65-e63b516d97ac")
	(cons "zwei" "87512329-a204-47e5-b38c-1b22838b6f7d")
	(cons "drei" "b77473f3-dba0-4b4f-9db7-3ba095d12de4")
	(cons "vier" "2a3d87d0-9ad0-416b-aa22-dea96fede8b7"))
  "Associating names of nodes with ids")


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
  :ID:       53e15dce-6f28-4674-bd65-e63b516d97ac
  :working-set-nodes:
  :END:
* zwei
  :PROPERTIES:
  :ID:       87512329-a204-47e5-b38c-1b22838b6f7d
  :END:
*************** Inline
  :PROPERTIES:
  :ID:       4939f218-d086-49c6-94f6-0f4046111b0f
  :END:
*************** END
* drei
  :PROPERTIES:
  :ID:       b77473f3-dba0-4b4f-9db7-3ba095d12de4
  :END:
** vier
   :PROPERTIES:
   :ID:       2a3d87d0-9ad0-416b-aa22-dea96fede8b7
   :END:
")
    (org-mode)
    (setq org-working-set-id "53e15dce-6f28-4674-bd65-e63b516d97ac")
    owst-work-buffer))


(provide 'owst)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; owst.el ends here
