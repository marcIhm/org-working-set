;;; org-id-cleanup.el --- Interactively cleanup unreferenced IDs of org-id     -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Marc Ihm <1@2484.de>
;; URL: https://github.com/marcIhm/org-working-set
;; Version: 1.1.0
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

;;   Version 1.1
;;
;;   - Respecting attachments
;;   - Refactoring
;;
;;   Version 1.0
;;
;;   - Initial Version
;;

;;; Code:

(require 'org)
(require 'button)
(require 'org-attach)


(defvar org-id-cleanup--all-steps '(backup save complete-files review-files collect-ids review-ids cleanup-ids save-again) "List of all supported steps.")
(defvar org-id-cleanup--initial-files nil "List of files to be scanned while cleaning ids as used by org-id alone.")
(defvar org-id-cleanup--all-files nil "List of all files to be scanned while cleaning ids.")
(defvar org-id-cleanup--unref-unattach-ids nil "List of IDs not referenced from files.  not having attachments.")
(defvar org-id-cleanup--num-deleted-ids 0 "Number of IDs deleted.")
(defvar org-id-cleanup--num-attach 0 "Number of IDs that are referenced by their attachment directory only.")
(defvar org-id-cleanup--num-all-ids 0 "Number of all IDs.")



;; User-visible function and dispatch
(defun org-id-cleanup ()
  "Find and clean ids that are no longer used, i.e. neither referenced by links nor used for attachments."
  (interactive)
  (org-id-cleanup--do nil 'backup))


(defun org-id-cleanup--do (come-from go-to)
  "Do the work for `org-id-cleanup' Argument COME-FROM is previous step or nil, GO-TO the next one or symbol previous or next."
  (let (step pt)

    ;; check arguments and compute step
    (if (and come-from
             (not (member come-from org-id-cleanup--all-steps)))
        (error "Internal error with come-from: %s" come-from))
    (unless (if come-from
                (member go-to '(previous next))
              (member go-to org-id-cleanup--all-steps))
      (error "Internal error with go-to: %s" go-to))
    (setq step
          (if come-from
              (nth (+ (if (eq go-to 'next) +1 -1)
                      (org-id-cleanup--step-to-num come-from)) org-id-cleanup--all-steps)
            go-to))
    (unless step (error "Internal error with step, come-from, go-to: %s/%s/%s" step come-from go-to))
    
    ;; prepare buffer
    (pop-to-buffer-same-window (get-buffer-create "*Assistant for deleting IDs*"))
    (setq buffer-read-only nil)
    (delete-other-windows)
    (erase-buffer)
    ;; breadcrumbs
    (let ((in-past-steps t))
      (dolist (st org-id-cleanup--all-steps)
        (insert (propertize (format "%s - " (symbol-name st)) 'face (if in-past-steps nil 'org-agenda-dimmed-todo-face)))
        (if (eq st step) (setq in-past-steps nil))))
    (backward-delete-char 3)
    
    (insert "\n\nThis assistant helps to clean up IDs from your org-files, it tries to remove only IDs, that are not referenced any longer.\n\n")

    ;; common controls
    (when (eq step 'backup)
      (insert "It operates in steps, and explains what is going to happen in each step; it presents buttons, that when pressed execute the described action and take you to the next step. Pressing a button can be done either with the return-key or with the mouse.")
      (fill-paragraph)
      (insert "\n\n"))
    (insert (format "Step %d of %s: %s"
                    (1+ (org-id-cleanup--step-to-num step))
                    (length org-id-cleanup--all-steps)
                    (symbol-name step)))
    (when (> (org-id-cleanup--step-to-num step) 0)
      (insert "   (or back to ")
      (insert-button
       (symbol-name (nth (1- (org-id-cleanup--step-to-num step)) org-id-cleanup--all-steps)) 'action
       (lambda (_) (org-id-cleanup--do step 'previous)))
      (insert ")"))
    (insert "\n\n")
    
    ;; prepare list of files for some steps
    (setq org-id-cleanup--initial-files
          (delete-dups
	   (mapcar #'file-truename
		   (append
		    ;; Agenda files and all associated archives
		    (org-agenda-files t org-id-search-archives)
		    ;; Explicit extra files
		    (unless (symbolp org-id-extra-files)
		      org-id-extra-files)
		    ;; All files known to have IDs
		    org-id-files))))

    ;; dispatch according to step
    ;; next step will be bound to button within each previous step, so no logic here
    (cond

     ((eq step 'backup)
      (org-id-cleanup--step-backup step))

     ((eq step 'save)
      (org-id-cleanup--step-save step))

     ((eq step 'complete-files)
      (org-id-cleanup--step-complete-files step org-id-cleanup--initial-files))

     ((eq step 'review-files)
      (org-id-cleanup--step-review-files step org-id-cleanup--all-files))

     ((eq step 'collect-ids)
      (org-id-cleanup--step-collect-ids step org-id-cleanup--all-files))

     ((eq step 'review-ids)
      (org-id-cleanup--step-review-ids step))
     
     ((eq step 'cleanup-ids)
      (org-id-cleanup--step-cleanup-ids step org-id-cleanup--unref-unattach-ids))

     ((eq step 'save-again)
      (org-id-cleanup--step-save-again))

     (t
      (error "Step %s not supported" step)))

    ;; finish buffer before leaving it to the user to press any buttons therein; see individual steps
    (recenter -1)
    (message "Please read comments and instructions and proceed by clicking the appriopriate buttons.")
    (setq buffer-read-only t)))


;; Individual steps
(defun org-id-cleanup--step-backup (this-step)
  "Step from `org-id-cleanup--do'.
Argument THIS-STEP contains name of current step."
  (insert "\nPlease make sure that you have a backup, if something goes wrong !\nThis assistant cannot do this for you; so please come back when done\nand press this ")
  (insert-button "button" 'action
                 (lambda (_) (org-id-cleanup--do this-step 'next))))


(defun org-id-cleanup--step-save (this-step)
  "Step from `org-id-cleanup--do'.
Argument THIS-STEP contains name of current step."
  (let (pt)

    (insert "You need to save all org buffers and update id locations: ")

    (insert-button
     "go" 'action
     (lambda (_)
       (goto-char (point-max))
       (setq buffer-read-only nil)
       (insert "\n\nSaving buffers ... ")
       (redisplay)
       (org-save-all-org-buffers)
       (insert "done\nUpdating id locations ... ")
       (redisplay)
       (org-id-update-id-locations)
       ;; continue with next step
       (org-id-cleanup--do this-step 'next)))

    (setq pt (point))
    (insert "\nThis will not change your org-files yet.")
    (goto-char pt)))


(defun org-id-cleanup--step-complete-files (this-step files)
  "Step from `org-id--cleanup-do.
Argument THIS-STEP contains name of current step, FILES is list of files to present to user for completion."
  (let ((head-of-files "--- start of extra files ---"))
    (insert (format "Complete the list of %d files that will be scanned and might be changed:\n\n" (length files)))
    (org-id-cleanup--insert-files files)
    (insert "\n\nPlease make sure, that this list is complete in two respects,\ni.e. includes all files that contain:\n\n - Nodes with IDs that will be removed if referenced no longer\n - References and Links to IDs\n\nPlease note: If the list above is incomplete regarding the second respect,\nthis will probably lead to IDs beeing removed, that are still referenced\nfrom a file missing in the list.")
    
    (insert "\nTo add files or directories to this list and only for this assistant, please ")
    (insert-button
     "browse" 'action
     (lambda (_)
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
     (lambda (_)
       ;; change global state
       (setq org-id-cleanup--all-files (sort (delete-dups (append files (org-id-cleanup--collect-extra-files head-of-files))) 'string<))
       ;; continue with next step
       (org-id-cleanup--do this-step 'next)))))


(defun org-id-cleanup--step-review-files (this-step files)
  "Step from `org-id--cleanup-do.
Argument THIS-STEP contains name of current step, FILES is the list of files to review."
  (let (pt)
    (insert (format "Review the list of %d files that will be scanned and might be changed:\n\n" (length files)))
    (org-id-cleanup--insert-files files)
    (insert "\n\nWhen satisfied ")

    (insert-button
     "continue" 'action
     (lambda (_)
       ;; continue with next step
       (org-id-cleanup--do this-step 'next)))

    (setq pt (point))
    (insert "\n\nPlease note, that this assistant will only recognize IDs as referenced and will refrain from deleting them, if they appear anywhere within your org-files. But if you use IDs from within your lisp-code, this will not be noticed. However, to protect such IDs it is enough to mention them anywhere within your org-files.")
    (fill-paragraph)
    (goto-char pt)))
     

(defun org-id-cleanup--step-collect-ids (this-step files)
  "Step from `org-id--cleanup-do.
Argument THIS-STEP contains name of current step, FILES is the list of files with IDs."
  (let ((counters (make-hash-table :test 'equal))
        (scanned 0)
        (attach 0)
        pgreporter unref unref-unattach)
    (insert (format "Now the relevant %d files will be scanned for IDs.\n\n" (length files)))
    (insert "Any IDs, that are used for attachment directories will be kept.\n\n")
    (insert "From now on, please refrain from leaving this assistant to create links to IDs, because they would not be taken into account any more.")
    (fill-paragraph)
    (insert "\n\nScan files for IDs and ")

    (insert-button
     "continue" 'action
     (lambda (_)
       (let (ids)
         (maphash (lambda (id _) (push id ids)) org-id-locations)
         (setq pgreporter (make-progress-reporter (format "Scanning %d files..." (length files)) 1 (length files)))
         (dolist (file files)
           (with-current-buffer (find-file-noselect file)
             (dolist (id ids)
               (goto-char (point-min))
               (while (search-forward id nil t)
                 (cl-incf (gethash id counters 0)))))
           (progress-reporter-update pgreporter (cl-incf scanned)))

         ;; keep only IDs, that have appeared only once
         (maphash (lambda (id count) (if (eq count 1) (push id unref))) counters)

         ;; keep only IDs, that are not used in attachment dir
         (dolist (id unref)
           (let ((pos (org-id-find id)))
             (with-current-buffer  (find-file-noselect (car pos))
               (goto-char (cdr pos))
               (if (string= (org-attach-dir-from-id id) (org-attach-dir))
                   (cl-incf attach)
                 (push id unref-unattach)))))

         (progress-reporter-done pgreporter)

         ;; change global state
         (setq org-id-cleanup--unref-unattach-ids unref-unattach)
         (setq org-id-cleanup--num-all-ids (length ids))
         (setq org-id-cleanup--num-attach attach)
         ;; continue with next step
         (org-id-cleanup--do this-step 'next))))))


(defun org-id-cleanup--step-review-ids (this-step)
  "Step from `org-id--cleanup-do'.
Argument THIS-STEP contains name of current step."
  (let ((head-of-ids "--- List of IDs to be deleted ---")
        pt pt2)
    (insert (format "Find below the list of IDs (%d out of %d) that will be deleted; pressing TAB on an id will show the respective node.\n" (length org-id-cleanup--unref-unattach-ids) org-id-cleanup--num-all-ids))
    (insert (format "%d IDs are not in the list and will be kept, because they have associated attachments.\n\n" org-id-cleanup--num-attach))
    (insert "You may remove IDs from the list as you like to keep them from beeing deleted.\n\n")
    (insert "If satisfied ")

    (insert-button
     "continue" 'action
     (lambda (_)
       (local-unset-key (kbd "<tab>")) ; tab is no longer needed in next step
       ;; change global state
       (setq org-id-cleanup--unref-unattach-ids (org-id-cleanup--collect-ids head-of-ids))
       ;; continue with next step
       (org-id-cleanup--do this-step 'next)))

    (setq pt (point))
    (insert "\n\n" head-of-ids "\n")
    (setq pt2 (point))
    (dolist (id org-id-cleanup--unref-unattach-ids)
      (insert id "\n"))
    (add-text-properties pt2 (point) '(inhibit-read-only t))
    (goto-char pt)
    (local-set-key (kbd "<tab>") 'org-id-cleanup--peek-into-id)))


(defun org-id-cleanup--step-cleanup-ids (this-step ids)
  "Step from `org-id--cleanup-do.
Argument THIS-STEP contains name of current step, IDS gived ids to remove."

  (let ((scanned 0)
        pgreporter)
    (insert "Please make sure, that you have not manually created new links referencing any IDs while the last two steps of this assistant were active.")
    (fill-paragraph)
    (insert (format "\n\nTo REMOVE %s IDs (out of %d) UNCONDITIONALLY, press this " (length org-id-cleanup--unref-unattach-ids) org-id-cleanup--num-all-ids))
    
    (insert-button
     "button" 'action
     (lambda (_)
       (setq buffer-read-only nil)
       (goto-char (point-max))
       (setq org-id-cleanup--num-deleted-ids 0)
       (insert "\n\nRemoving unused IDs ... ")
       (redisplay)
       (setq pgreporter (make-progress-reporter (format "Removing %d IDs..." (length org-id-cleanup--unref-unattach-ids)) 1 (length org-id-cleanup--unref-unattach-ids)))

       (dolist (id ids)
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

       ;; change global state
       (setq org-id-cleanup--unref-unattach-ids nil)

       ;; continue with next step
       (org-id-cleanup--do this-step 'next)))))


(defun org-id-cleanup--step-save-again ()
  "Step from `org-id--cleanup-do'."
  (insert (format "Deleted %d IDs (out of %d).\n\n" org-id-cleanup--num-deleted-ids org-id-cleanup--num-all-ids))
  (insert "Finally you should again save all org buffers, update id locations and save them: ")

  (insert-button
   "go" 'action
   (lambda (_)
     (setq buffer-read-only nil)
     (goto-char (point-max))
     (insert "\n\nSaving buffers ... ")
     (redisplay)
     (org-save-all-org-buffers)
     (insert "done\nUpdating ids ... ")
     (redisplay)
     (org-id-update-id-locations org-id-cleanup--all-files)
     (insert "done\nSaving id locations ...")
     (redisplay)
     (org-id-locations-save)
     
     (insert "done\n\nAssistant done.\n")
     (setq buffer-read-only t))))


;; Some helper functions
(defun org-id-cleanup--insert-files (files)
  "Insert given list of FILES into current buffer using full window width."
  (let ((tab-stop-list '(2 42 82)))
    (dolist (name files)
      (if (> (+ (indent-next-tab-stop (current-column))
                (length name))
             (- (window-width) 10))
          (insert "\n"))
      (tab-to-tab-stop)
      (insert name))))


(defun org-id-cleanup--collect-extra-files (head)
  "Collect and return edited list of extra file.
Argument HEAD is a marker-string that precedes the list of files in buffer."
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
  "Collect and return edited list of IDs.
Argument HEAD is a marker-string, that precedes the list of ids in buffer."
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
          (error "Id %s does not seem to be a valid uuid" id))
        (push id ids))
      (forward-line))
    ids))


(defun org-id-cleanup--peek-into-id ()
  "Show node with if of current line in other window."
  (interactive)
  (let* ((id (string-trim (buffer-substring (point-at-bol) (point-at-eol))))
         (marker (org-id-find id t)))
    (unless marker
      (error "Cannot find ID %s" id))
    (pop-to-buffer (marker-buffer marker))
    (goto-char marker)
    (search-forward id)
    (beginning-of-line)
    (org-show-context 'tree)
    (recenter)
    (other-window 1)
    (message "Context of node with id %s" id)))


(defun org-id-cleanup--step-to-num (step)
  "Return number of current STEP within list of all steps (counting from 0)."
  (- (length org-id-cleanup--all-steps)
     (length (member step org-id-cleanup--all-steps))))


(provide 'org-id-cleanup)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; org-id-cleanup.el ends here
