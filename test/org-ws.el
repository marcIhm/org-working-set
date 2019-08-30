;;; org-wst.el --- Regression Tests for org-working-set.el

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


(defvar oidxt-saved-state nil "Store state of customizable variables")
(defvar oidxt-ert-index-file (concat temporary-file-directory "oidxt-ert-index.org"))
(defvar oidxt-ert-work-file (concat temporary-file-directory "oidxt-ert-work.org"))
(defvar oidxt-work-buffer nil)
(defvar oidxt-index-buffer nil)
(defvar oidxt-saved-id nil)
(defvar oidxt-saved-id-locations nil)
(defvar oidxt-saved-agenda-files nil)
(defvar oidxt-keep-test-state nil)

;;
;; All tests
;;

(ert-deftest oidxt-test-clock-into-working-set ()
  (oidxt-with-test-setup
    (unwind-protect
	(progn
	  (let ((oidx--after-ws-delay 1) (org-index-clock-into-working-set nil))
	    (should (not (org-clock-is-active)))
	    (oidxt-do "o c c u r <return> z w e i <down> <return>")
	    (oidxt-do "w o r k i n g - s e t <return> s")
	    (sleep-for 2)
	    (should (not (org-clock-is-active)))
	    
	    (oidxt-do "w o r k i n g - s e t <return> w")
            (execute-kbd-macro (kbd "<S-return>"))
	    (sleep-for 2)
	    (should (not (org-clock-is-active)))
	    
	    (setq org-index-clock-into-working-set t)
	    (should (not (org-clock-is-active)))
	    (oidxt-do "o c c u r <return> z w e i <down> <return>")
	    (oidxt-do "w o r k i n g - s e t <return> s")
	    (sleep-for 2)
	    (should (org-clock-is-active))))
      (org-clock-out))))


(ert-deftest oidxt-test-working-set ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> z w e i <down> <return>")
    (oidxt-do "w o r k i n g - s e t <return> s")
    (beginning-of-buffer)
    (oidxt-do "w o r k i n g - s e t <return>" "C-u")
    (should (looking-at ".* --8--"))))


(ert-deftest oidxt-test-working-set-do-not-clock ()
  (oidxt-with-test-setup
    (should (not (org-clock-is-active)))
    (oidxt-do "o c c u r <return> z w e i <down> <return>")
    (should (not oidx--ws-ids-do-not-clock))
    (oidxt-do "w o r k i n g - s e t <return> S")
    (should oidx--ws-ids-do-not-clock)
    (should (not (org-clock-is-active)))))


(ert-deftest oidxt-test-working-set-restore ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> z w e i <down> <return>")
    (oidxt-do "w o r k i n g - s e t <return> s")
    (should (= (length oidx--ws-ids) 1))
    (oidxt-do "w o r k i n g - s e t <return> d")
    (should (= (length oidx--ws-ids) 0))
    (oidxt-do "w o r k i n g - s e t <return> u")
    (should (= (length oidx--ws-ids) 1))))


(ert-deftest oidxt-test-working-set-bottom-head ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> - - 4 - - <return>")
    (oidxt-do "w o r k i n g - s e t <return> s")
    (beginning-of-buffer)
    (oidxt-do "w o r k i n g - s e t <return> b" "C-u")
    (forward-line)
    (should (looking-at ".* --2--"))
    (forward-line -1)
    (oidxt-do "w o r k i n g - s e t <return> h" "C-u")
    (should (looking-at ".* --4--"))))


(ert-deftest oidxt-test-working-set-menu-goto ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> z w e i <down> <return>")
    (oidxt-do "w o r k i n g - s e t <return> s")
    (oidxt-do "o c c u r <return> e i n s <down> <return>")
    (oidxt-do "w o r k i n g - s e t <return> a")
    (oidxt-do "w o r k i n g - s e t <return> m <down> <return>")
    (should (looking-at ".* --8--"))))


(ert-deftest oidxt-test-working-set-menu-delete ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> z w e i <down> <return>")
    (oidxt-do "w o r k i n g - s e t <return> s")
    (oidxt-do "o c c u r <return> e i n s <down> <return>")
    (oidxt-do "w o r k i n g - s e t <return> a")
    (should (= (length oidx--ws-ids) 2))
    (oidxt-do "w o r k i n g - s e t <return> m <down> d q")
    (should (= (length oidx--ws-ids) 1))))


(ert-deftest oidxt-test-working-set-menu-toggle-clocking ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> z w e i <down> <return>")
    (oidxt-do "w o r k i n g - s e t <return> s")
    (oidxt-do "w o r k i n g - s e t <return> m ~")
    (should (looking-at "\\*~"))
    (execute-kbd-macro (kbd "c"))
    (should (looking-at "\\* "))))


(ert-deftest oidxt-test-double-working-set ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> z w e i <down> <return>")
    (oidxt-do "w o r k i n g - s e t <return> s")
    (oidxt-do "o c c u r <return> e i n s <down> <return>")
    (oidxt-do "w o r k i n g - s e t <return> a")
    (oidxt-do "w o r k i n g - s e t <return> SPC SPC")
    (should (looking-at ".* --8--"))
    (oidxt-do "w o r k i n g - s e t <return> SPC")
    (should (looking-at ".* --8--"))
    (oidxt-do "w o r k i n g - s e t <return> SPC SPC")
    (should (looking-at ".* --13--"))))


(ert-deftest oidxt-test-nested-working-set ()
  (oidxt-with-test-setup
    (oidxt-do "o c c u r <return> v i e r <down> <return>")
    (oidxt-do "w o r k i n g - s e t <return> s")
    (search-forward "neun")
    (org-reveal)
    (oidxt-do "w o r k i n g - s e t <return> a")
    (should (= (length oidx--ws-ids) 1))))


;;
;; Helper functions
;;

(defmacro oidxt-with-test-setup (&rest body)
  "Execute body within test setup"
  (declare (indent 0) (debug t))
  `(progn
     (oidxt-setup-test)
     (unwind-protect
         (progn ,@body)
       (oidxt-teardown-test))))


(defun oidxt-do (keys &optional prefix)
  (execute-kbd-macro (kbd (concat prefix (if prefix " " "") "M-x o i d x - - do <return> " keys))))


(defun oidxt-get-refs ()
  (let (refs ref-field)
    (with-current-buffer oidx--buffer
      (oidx--go-below-hline)
      (while (org-at-table-p)
        (setq ref-field (oidx--get-or-set-field 'ref))
        (string-match oidx--ref-regex ref-field)
        (setq refs (cons
                    (string-to-number (match-string 1 ref-field))
                    refs))
        (forward-line)))
    (pp refs t)
    refs))


(defun oidxt-setup-test ()
  (interactive)
  (if oidx--sort-timer
      (cancel-timer oidx--sort-timer))
  (if (get-buffer "*org-index-occur*") (kill-buffer "*org-index-occur*"))
  (setq oidx--last-sort-assumed 'mixed)
  (setq oidx--maxrefnum nil)
  (setq oidx--occur-assert-result t)
  ;; remove any left over buffers
  (oidxt-remove-work-buffers)
  ;; create them new
  (switch-to-buffer oidxt-work-buffer)
  (oidxt-create-work-buffer)
  (oidxt-prepare-test-index)
  (setq oidx--last-sort org-index-sort-by)
  (setq oidx--ws-ids nil)
  (setq oidx--ws-ids-do-not-track nil)
  (switch-to-buffer oidxt-work-buffer)
  (basic-save-buffer)
  (org-agenda-file-to-front oidxt-ert-work-file)
  (org-cycle '(16))
  (delete-other-windows)
  (end-of-buffer)
  (forward-line -2))


(defun oidxt-teardown-test ()
  (interactive)
  (remove-hook 'before-save-hook 'oidx--sort-silent)
  (if (not oidxt-keep-test-state) (oidxt-restore-saved-state))
  (with-current-buffer oidxt-work-buffer (set-buffer-modified-p nil))
  (with-current-buffer oidxt-index-buffer (set-buffer-modified-p nil))
  (org-remove-file oidxt-ert-work-file)
  (setq oidx--head nil))


(defun oidxt-remove-work-buffers ()
  "Remove any left over work buffers"
  (mapc (lambda (x)
          (let ((b (get-buffer x)))
            (when b
              (with-current-buffer b
                (set-buffer-modified-p nil))
              (kill-buffer b))))
        (list "oidxt-ert-index.org"
              "oidxt-ert-work.org"))
  (setq oidxt-work-buffer nil
        oidxt-index-buffer nil))


(defun oidxt-save-and-set-state (new-id)
  (let (customizable)

    ;; get customizable variables (they have property standard-value)
    (mapatoms (lambda (x) (if (and (string-match "^org-index-.*"
						 (symbol-name x))
				   (custom-variable-p x))
			      (setq customizable (cons x customizable)))))

    ;; save all customizable variables
    (unless oidxt-saved-state
      (setq oidxt-saved-state
            (mapcar (lambda (x)
                      (cons x (and (boundp x)
				   (symbol-value x))))
                    customizable)))

    ;; set them all to their standard values
    (mapcar (lambda (x)
              (set x (eval (car (get x 'standard-value)))))
            customizable)

    ;; save some standard org-variables
    (unless oidxt-saved-id (setq oidxt-saved-id org-index-id))
    (setq org-index-id new-id)

    (unless oidxt-saved-id-locations (setq oidxt-saved-id-locations org-id-locations))
    (setq org-id-locations nil)

    (unless oidxt-saved-agenda-files org-agenda-files)
    (setq org-agenda-files nil)))


(defun oidxt-restore-saved-state ()
  (if oidxt-saved-state
      (mapc (lambda (x) (set (car x) (cdr x))) oidxt-saved-state)
    (error "No saved state to restore"))

  (when oidxt-saved-id
    (setq org-index-id oidxt-saved-id)
    (setq oidxt-saved-id nil))
  
  (when oidxt-saved-id-locations
    (setq org-id-locations oidxt-saved-id-locations)
    (setq oidxt-saved-id-locations nil))

  (when oidxt-saved-agenda-files
    (setq org-agenda-files oidxt-saved-agenda-files)
    (setq oidxt-saved-agenda-files)))


;;
;; Test data
;;

(defun oidxt-prepare-test-index ()
  (let ((test-id "1f44f43c-1a37-4d55-oidxt-test-index"))
    (oidxt-save-and-set-state test-id)
    (remove-hook 'before-save-hook 'oidx--sort-silent)
    (org-id-add-location test-id oidxt-ert-index-file)
    (setq org-index-occur-columns 8)
    (unless oidxt-index-buffer
      (setq oidxt-index-buffer (find-file-noselect oidxt-ert-index-file)))
    (with-current-buffer oidxt-index-buffer
      (setq buffer-save-without-query t)
      (auto-save-mode t) ; actually disables
      (if (file-exists-p buffer-auto-save-file-name)
          (delete-file buffer-auto-save-file-name))
      (erase-buffer)
      (org-mode)
      (insert 

       "* oidxt-test-index
  :PROPERTIES:
  :ID:       " test-id "
  :max-ref:  --14--
  :END:
       

  |    ref | id                                   | created         | category | level | count | last-accessed | keywords       | yank | tags |
  |        | <4>                                  |                 |          |       |       |               |                |      |      |
  |--------+--------------------------------------+-----------------+----------+-------+-------+---------------+----------------+------+------|
  | --14-- |                                      | [2013-12-19 Do] |          |       |     1 |               |                |      |      |
  | --13-- | 5a16c863-1f7e-4636-9c47-74e4d49f72df | [2013-12-19 Do] |          |       |     1 |               | eins           |      |      |
  | --12-- |                                      | [2013-12-19 Do] |          |       |     1 |               | vier-zwei      |      |      |
  | --11-- |                                      | [2013-12-19 Do] |          |       |     1 |               | vier-eins      |      |      |
  | --10-- |                                      | [2013-12-19 Do] |          |       |     1 |               | vier           |      |      |
  |  --9-- |                                      | [2013-12-19 Do] |          |       |     1 |               | drei           |      |      |
  |  --8-- | 588bda71-38b7-41a9-90f0-cc9fb39991fa | [2013-12-19 Do] |          |       |     1 |               | zwei-zwei-eins |      |      |
  |  --7-- |                                      | [2013-12-19 Do] |          |       |     1 |               | zwei-zwei      |      |      |
  |  --6-- |                                      | [2013-12-19 Do] |          |       |     1 |               | zwei-eins      |      |      |
  |  --5-- |                                      | [2013-12-19 Do] |          |       |     1 |               | zwei           |      |      |
  |  --4-- | 12ae411f-bdd4-4c92-9e24-75cf7858f586 | [2013-12-19 Do] |          |       |     1 |               | eins-drei      |      |      |
  |  --3-- |                                      | [2013-12-19 Do] |          |       |     1 | [2013-12-19 Do 10:00]              | eins-zwei      |      |      |
  |  --2-- | caac71f6-74fa-4b6a-b732-66c9ceb0c483 | [2013-12-19 Do] |          |       |     1 |               | eins-eins      |      |      |
  |  --1-- | " test-id "                          | [2013-12-15 So] |          |       |     1 | [2013-12-15 So 10:00] | This node      |      |      |

")
      (forward-line -1)
      (basic-save-buffer)
      (org-id-update-id-locations (list oidxt-ert-work-file) t)
      (puthash test-id oidxt-ert-index-file org-id-locations)
      (setq oidx--head nil)
      (org-table-align))))


(defun oidxt-create-work-buffer ()
  (unless oidxt-work-buffer
    (setq oidxt-work-buffer (find-file-noselect oidxt-ert-work-file)))
  (with-current-buffer oidxt-work-buffer
    (setq buffer-save-without-query t)
    (auto-save-mode t) ; actually disables
    (if (file-exists-p buffer-auto-save-file-name)
        (delete-file buffer-auto-save-file-name))
    (erase-buffer)
    (insert "* --1-- eins
* --8-- acht
  :PROPERTIES:
  :ID:       588bda71-38b7-41a9-90f0-cc9fb39991fa
  :END:
* --13--
  :PROPERTIES:
  :ID:       5a16c863-1f7e-4636-9c47-74e4d49f72df
  :END:
* vier --4--
  :PROPERTIES:
  :ID:       12ae411f-bdd4-4c92-9e24-75cf7858f586
  :END:

  Zeile 1

*************** Inline
*************** END

  Zeile 2

* --2-- zwei --2--
  :PROPERTIES:
  :ID:       caac71f6-74fa-4b6a-b732-66c9ceb0c483
  :org-index-ref: foo
  :END:
* drei
** neun
")
    (org-mode)
    oidxt-work-buffer))


(provide 'oidxt)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; oidxt.el ends here
