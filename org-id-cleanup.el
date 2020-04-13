;;; org-id-cleanup.el --- Interactively cleanup unreferenced IDs of org-id     -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Marc Ihm <1@2484.de>
;; URL: https://github.com/marcIhm/org-working-set
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:

;; Purpose:
;;
;;  Interactively cleanup IDs which have been created by org-id but
;;  are no longer referenced from anywhere else in org.
;;
;;  Normal usage of org-id does not lead to a lot of unreferenced IDs,
;;  and org-id normally does not suffer from them.
;;  However, some packages like org-working-set lead to such IDs during
;;  notmal usage; in such cases it might be helpful clean up.
;;

;;; Change Log:

;;   Version 1.0
;;
;;   - Initial Version
;;

;;; Code:

(require 'org)
(require 'button)


(defvar org-id-cleanup--extra-files nil "List of all files to be scanned while cleaning ids.")
(defvar org-id-cleanup--unref-ids nil "List of IDs not referenced from files.")
(defvar org-id-cleanup--num-unref-ids 0 "Number of IDs not referenced from files.")
(defvar org-id-cleanup--num-deleted-ids 0 "Number of IDs deleted.")
(defvar org-id-cleanup--num-all-ids 0 "Number of all IDs.")


(defun org-id-cleanup ()
  "Find and clean ids that are no longer appear anywhere"
  (interactive)
  (org-id-cleanup--do 1))


(defun org-id-cleanup--do (step)
  "Do the work for `org-id-cleanup'"
  (let* ((counters (make-hash-table :test 'equal))
         ;; below is copied from org-id
         (files (delete-dups
		 (mapcar #'file-truename
			 (append
			  ;; Agenda files and all associated archives
			  (org-agenda-files t org-id-search-archives)
			  ;; Explicit extra files
			  (unless (symbolp org-id-extra-files)
			    org-id-extra-files)
			  ;; All files known to have IDs
			  org-id-files))))
         (nfiles (length files))
         (scanned 0)
         (queried 0)
         (buna "*Assistant for deleting IDs*")
         (head-of-files "--- start of extra files ---")
         (head-of-ids "--- List of IDs to be deleted ---")
         (sample-uuid (org-id-uuid))
         pgreporter unref pt pt2)

    (pop-to-buffer-same-window (get-buffer-create buna))
    (setq buffer-read-only nil)
    (delete-other-windows)
    (erase-buffer)
    (insert "\nThis assistant helps to clean up IDs from your org-files, it tries to remove only IDs, that are not referenced any longer.\n\n")
    
    (when (= step 1)
      (insert "It operates in steps, and explains what is going to happen in each step; it presents buttons, that when pressed execute the described action and take you to the next step. Pressing a button can be done either with the cursor and the return-key or with the mouse.")
      (fill-paragraph)
      (insert "\n\n"))
    (insert (format "Step %d of 8" step))

    (when (> step 1)
      (insert "   (or ")
      (insert-button
       "back" 'action
       (lambda (btn) (org-id-cleanup--do (- step 1))))
      (insert ")"))

    (when (> step 3)
      (setq files (sort (delete-dups (append files org-id-cleanup--extra-files)) 'string<))
      (setq nfiles (length files)))
    
    (insert "\n\n")
    
    (cond

     ((eq step 1)
      (insert "\nPlease make sure that you have a backup, if something goes wrong !\nThis assistant cannot do this for you; please come back when done\nand press this ")
      (insert-button "button" 'action
                     (lambda (btn) (org-id-cleanup--do 2))))

     ((eq step 2)
      (insert "You need to save all org buffers and update id locations: ")
      (insert-button
       "go" 'action
       (lambda (btn)
         (goto-char (point-max))
         (setq buffer-read-only nil)
         (insert "\n\nSaving buffers ... ")
         (redisplay)
         (org-save-all-org-buffers)
         (insert "done\nUpdating ids ... ")
         (redisplay)
         (org-id-update-id-locations)         
         (org-id-cleanup--do 3))))

     ((or (eq step 3))
      (insert (format "Complete the list of %d files that might be changed:\n\n" (length files)))
      (org-id-cleanup--insert-files files)
      (insert "\n\nPlease make sure, that this list is complete in two respects,\ni.e. includes all files that contain:\n\n - Nodes with IDs that will be removed if referenced no longer\n - References and Links to IDs\n\nPlease note: If the list above is incomplete regarding the second respect,\nthis will probably lead to IDs beeing removed, that are still referenced\nfrom a file missing in the list.")

      (insert "\nTo add files or directories to this list and only for this assistant, please ")
      (insert-button
       "browse" 'action
       (lambda (btn)
         (let ((file (read-file-name "Choose a single files or a whole directory: " org-directory)))
           (when file
             (setq buffer-read-only nil)
             (goto-char (point-max))
             (search-backward head-of-files)
             (end-of-line)
             (insert "\n" file)
             (setq buffer-read-only t)))))
      (insert "\n\n" head-of-files "\n")
      (insert "---  end  of extra files ---\n")      

      (insert "\nAfter doing that, you might want to ")
      (insert-button
       "continue" 'action
       (lambda (btn)
         (setq org-id-cleanup--extra-files
               (org-id-cleanup--collect-extra-files head-of-files))
         (org-id-cleanup--do 4))))

     ((eq step 4)
      (insert (format "Review the list of %d files, that might be changed:\n\n" nfiles))
      (org-id-cleanup--insert-files files)
      (insert "\n\nWhen satisfied ")
      (insert-button
       "continue" 'action (lambda (btn) (org-id-cleanup--do 5))))
     
     ((eq step 5)
      (insert (format "Now the relevant %d files will be scanned for IDs.\n\n" nfiles))
      (insert-button
       "Scan files for IDs and continue" 'action
       (lambda (btn)
         (let (ids)
           (maphash (lambda (k v) (push k ids)) org-id-locations)
           (setq pgreporter (make-progress-reporter (format "Scanning %d files..." nfiles) 1 nfiles))
           (dolist (file files)
             (with-current-buffer (find-file-noselect file)
               (dolist (id ids)
                 (goto-char (point-min))
                 (while (search-forward id nil t)
                   (cl-incf (gethash id counters 0)))))
             (progress-reporter-update pgreporter (cl-incf scanned)))
           (maphash (lambda (k v) (if (eq v 1) (push k unref))) counters)
           (setq org-id-cleanup--unref-ids unref)
           (setq org-id-cleanup--num-unref-ids (length unref))
           (setq org-id-cleanup--num-all-ids (length ids))
           (progress-reporter-done pgreporter)
           (org-id-cleanup--do 6)))))

     ((eq step 6)
      (insert (format "Find below the list of IDs (%d out of %d) that will be deleted; pressing TAB on an id will show the respective node.\n" org-id-cleanup--num-unref-ids org-id-cleanup--num-all-ids))
      (insert "You may remove IDs from the list as you like to keep them from beeing deleted.\n\n")
      (insert "If satisfied ")
      (insert-button
       "continue" 'action
       (lambda (btn)
         (local-unset-key (kbd "<tab>"))
         (setq org-id-cleanup--unref-ids (org-id-cleanup--collect-ids head-of-ids))
         (setq org-id-cleanup--num-unref-ids (length org-id-cleanup--unref-ids))
         (org-id-cleanup--do 7)))
      (setq pt (point))
      (insert "\n\n" head-of-ids "\n")
      (setq pt2 (point))
      (dolist (id org-id-cleanup--unref-ids)
        (insert id "\n"))
      (add-text-properties pt2 (point) '(inhibit-read-only t))
      (goto-char pt)
      (local-set-key
       (kbd "<tab>")
       (lambda () (interactive)
         (let* ((id (string-trim (buffer-substring (point-at-bol) (point-at-eol))))
                (marker (org-id-find id t)))
           (unless marker
             (error "Cannot find ID %s" id))
           (unless (= (length id) (length sample-uuid))
             (error "ID %s does not seem to be a valid uuid" id))
           (pop-to-buffer (marker-buffer marker))
           (goto-char marker)
           (search-forward id)
           (beginning-of-line)
           (org-show-context 'tree)
           (recenter)
           (other-window 1)
           (message "Context of node with id %s" id)))))
     
     ((eq step 7)
      (insert (format "To REMOVE %s IDs (out of %d) UNCONDITIONALLY, press this " org-id-cleanup--num-unref-ids org-id-cleanup--num-all-ids))
      (insert-button
       "button" 'action
       (lambda (btn)
         (setq buffer-read-only nil)
         (goto-char (point-max))
         (setq org-id-cleanup--num-deleted-ids 0)
         (insert "\n\nRemoving unused IDs ... ")
         (redisplay)
         (setq pgreporter (make-progress-reporter (format "Removing %d IDs..." org-id-cleanup--num-unref-ids) 1 org-id-cleanup--num-unref-ids))
         (dolist (id org-id-cleanup--unref-ids)
           (pop-to-buffer (find-file-noselect (gethash id org-id-locations)))
           (goto-char (point-min))
           (search-forward id)
           (unless (string= id (org-id-get))
             (error "Expected id of this node to be %s, but found %s" id (org-id-get)))
           (org-delete-property "ID")
           (cl-incf org-id-cleanup--num-deleted-ids)
           (progress-reporter-update pgreporter (cl-incf scanned)))
         (progress-reporter-done pgreporter)
         (sleep-for 1)
         (setq org-id-cleanup--unref-ids nil)
         (org-id-cleanup--do 8))))

     ((eq step 8)
      (insert (format "Deleted %d IDs (out of %d).\n\n" org-id-cleanup--num-deleted-ids org-id-cleanup--num-all-ids))
      (insert "Finally you should again save all org buffers, update id locations and save them: ")
      (insert-button
       "Go" 'action
       (lambda (btn)
         (setq buffer-read-only nil)
         (goto-char (point-max))
         (insert "\n\nSaving buffers ... ")
         (redisplay)
         (org-save-all-org-buffers)
         (insert "done\nUpdating ids ... ")
         (redisplay)
         (org-id-update-id-locations)
         (insert "done\nSaving id locations ...")
         (redisplay)
         (org-id-locations-save)
         
         (insert "done\n\nAssistant done.\n")
         (setq buffer-read-only t))))

     (t
      (error "Step %s not supported" step)))

    (recenter -1)
    (message "Please read comments and instructions and proceed by clicking the appriopriate buttons.")
    (setq buffer-read-only t)))


(defun org-id-cleanup--insert-files (files)
  "Insert given list of files into current buffer"
  (let ((tab-stop-list '(2 42 82)))
    (dolist (name files)
      (if (> (+ (indent-next-tab-stop (current-column))
                (length name))
             (- (window-width) 10))
          (insert "\n"))
      (tab-to-tab-stop)
      (insert name))))


(defun org-id-cleanup--collect-extra-files (head)
  "Collect edited list of files"
  (let (file files)
    (goto-char (point-min))
    (search-forward head)
    (delete-trailing-whitespace (point) (point-max))
    (forward-line)
    (while (not (looking-at "---"))
      (setq file (buffer-substring (point) (point-at-eol)))
      (cond
       ((file-directory-p file)
        (setq files (append files (directory-files file t org-agenda-file-regexp))))
       ((file-exists-p file)
        (push file files))
       (t (error "%s is neither a file nor a directory" file)))
      (forward-line))
    (delete-dups (mapcar #'file-truename files))))


(defun org-id-cleanup--collect-ids (head)
  "Collect edited list of IDs"
  (let ((sample-uuid (org-id-uuid))
        id ids)
    (goto-char (point-min))
    (search-forward head)
    (delete-trailing-whitespace (point) (point-max))
    (forward-line)
    (while (not (= (point) (point-max)))
      (setq id (string-trim (buffer-substring (point-at-bol) (point-at-eol))))
      (when (> (length id) 0)
        (unless (= (length id) (length sample-uuid))
          (error "id %s does not seem to be a valid uuid" id))
        (push id ids))
      (forward-line))
    ids))


(provide 'org-id-cleanup)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; org-id-cleanup.el ends here
