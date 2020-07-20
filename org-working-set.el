;;; org-working-set.el --- Manage a working-set of org-nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Marc Ihm <1@2484.de>
;; URL: https://github.com/marcIhm/org-working-set
;; Version: 2.3.0
;; Package-Requires: ((org "9.2.6") (dash "2.12.0") (s "1.12.0") (emacs "25.1"))

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
;;  Maintain a small and changing subset of your org-nodes to visit with ease.
;;
;;  The working-set is a small set of nodes, among which you can switch
;;  rapidly; it is expected to change on a daily or even hourly basis.  Put
;;  nodes into your working set in order to return easily after any
;;  interruption.
;;
;;  Once you have added nodes to your working set, there are two ways to
;;  traverse them (both are accessible through the central function
;;  `org-working-set'): cycling through your working set is the quickest
;;  way to return to the current node or go to others; alternatively,
;;  invoking the working set menu allows for better control but may require
;;  more keystrokes.
;;  
;;  Please note, that org-working-set adds an id-property to all nodes in
;;  the working-set.
;;
;;  The list of ids from the nodes of your working-set is stored within the
;;  property-drawer of a distinguished node specified via
;;  `org-working-set-id'; this can be any node you choose and is itself not
;;  part of the working-set.
;;
;;  Remark: Depending on your needs you might also find these packages
;;  interesting for providing somewhat similar functionality: org-now and
;;  org-mru-clock.
;;
;; Setup:
;;
;;  - org-working-set can be installed with package.el
;;  - Invoke `org-working-set', it will explain and assist you to set the
;;    customizable variable `org-working-set-id'
;;

;;; Change Log:

;;   Version 2.3
;;
;;   - Renamed 'log of working-set nodes' into 'journal'
;;   - Create org-working-set-dispatch-keymap for easier customization
;;   - In-prompt display of some settings
;;
;;   Version 2.2
;;
;;   - Moved org-id-cleanup to its own package
;;   - Improved handling of missing ids in working set
;;   - Refactoring
;;   - Fixes
;;
;;   Version 2.1
;;
;;   - Added org-id-cleanup to clean up unreferenced IDs without attachments
;;
;;   Version 2.0
;;
;;   - Added a log of working set nodes
;;   - The node designated by org-working-set-id will be used to store this log
;;   - Simplified handling of clocking
;;   - Retired property working-set-nodes-do-not-clock
;;   - Renamed custom-variable org-working-set-clock-into-working-set into
;;     org-working-set-clock-in
;;   - Renamed org-working-set-show-working-set-overlay into
;;     org-working-set-show-overlay
;;   - Renamed org-working-set-goto-bottom-in-working-set into
;;     org-working-set-goto-bottom
;;
;;   Version 1.1
;;
;;   - Moved functions for working set into its own file
;;   - Show breadcrumbs in working-set-menu
;;   - Prepare for melpa
;;
;;  See the package org-index for older news

;;; Code:

(require 'org)
(require 'dash)
(require 's)


;;; customizable options

(defgroup org-working-set nil
  "Options concerning the working-set of org-nodes; see `org-working-set' for details."
  :tag "Org Working-set"
  :group 'org)

(defcustom org-working-set-id nil
  "Id of the Org-mode node, which contains the index table.  This should be set to the id of an empty node.  The property drawer will be used to store the ids of the working-set nodes, the body will be populated with an ever-growing list of nodes, that have been added."
  :group 'org-working-set
  :type 'string)

(defcustom org-working-set-clock-in nil
  "Clock into nodes of working-set ?"
  :group 'org-working-set
  :type 'boolean)

(defcustom org-working-set-land-at-end nil
  "When visiting a node, land at end ?"
  :group 'org-working-set
  :type 'boolean)


;;; Variables

(defvar org-working-set--ids nil "Ids of working-set nodes (if any).")
(defvar org-working-set--ids-saved nil "Backup for ‘org-working-set--ids’.")
(defvar org-working-set--id-last-goto nil "Id of last node from working-set, that has been visited.")
(defvar org-working-set--circle-before-marker nil "Marker for position before entry into circle.")
(defvar org-working-set--circle-win-config nil "Window configuration before entry into circle.")
(defvar org-working-set--last-message nil "Last message issued by working-set commands.")
(defvar org-working-set--circle-cancel-transient-function nil "Function to end circle.")
(defvar org-working-set--cancel-timer nil "Timer to cancel waiting for key.")
(defvar org-working-set--overlay nil "Overlay to display name of current working-set node.")
(defvar org-working-set--short-help-wanted nil "Non-nil, if short help should be displayed in working-set menu.")
(defvar org-working-set--id-not-found nil "Id of last node not found.")
(defvar org-working-set--clock-in-curr nil "Current and effecive value of `org-working-set-clock-in'")
(defvar org-working-set--land-at-end-curr nil "Current and effecive value of `org-working-set-land-at-end'")

(defun org-working-set--define-keymap (keymap keylist)
  "Define Keys given by KEYLIST in KEYMAP."
  (dolist (keyentry keylist)
    (dolist (key (car keyentry))
      (define-key keymap (kbd key) (cdr keyentry))))
  keymap)

(defvar org-working-set-dispatch-keymap
  (let ((keymap (make-sparse-keymap)))
    (org-working-set--define-keymap
     keymap
     '((("s") . org-working-set--set)
       (("a") . org-working-set--add)
       (("d") . org-working-set--delete-from)
       (("SPC") . org-working-set--menu)
       (("TAB" "<tab>") . org-working-set--circle-start)
       (("?") . org-working-set--dispatch-toggle-help)
       (("j") . org-working-set--journal-enter)
       (("c") . org-working-set--dispatch-toggle-clock-in)
       (("l") . org-working-set--dispatch-toggle-land-at-end)
       (("u") . org-working-set--nodes-restore)
       (("C-g" "q") . keyboard-quit))))
  "Keymap used for initial dispatch after calling `org-working-set'.")

(defvar org-working-set-circle-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap org-mode-map)
    (org-working-set--define-keymap
     keymap
     '((("TAB" "<tab>") . org-working-set--circle-forward)
       (("c") . org-working-set--circle-toggle-clock-in)
       (("l") . org-working-set--circle-toggle-land-at-end)
       (("RET" "q") . org-working-set--circle-done)
       (("DEL") . org-working-set--circle-backward)
       (("?") . org-working-set--circle-toggle-help)
       (("d") . org-working-set--circle-delete-current)
       (("C-g" "q") . org-working-set--circle-quit))))
  "Keymap used in working set circle.")

(defvar org-working-set-menu-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap org-mode-map)
    (org-working-set--define-keymap
     keymap
     '((("RET" "SPC") . org-working-set-menu-go--this-win)
       (("TAB" "<tab>") . org-working-set-menu-go--other-win)
       (("p") . org-working-set--menu-peek)
       (("d") . org-working-set--menu-delete-entry)
       (("u") . org-working-set--menu-undo)
       (("q") . org-working-set--menu-quit)
       (("c") . org-working-set--menu-toggle-clock-in)
       (("l") . org-working-set--menu-toggle-land-at-end)
       (("?") . org-working-set--menu-toggle-help)
       (("r") . org-working-set--menu-rebuild))))
  "Keymap used in working set menu.")

(defvar org-working-set--dispatch-help-strings nil "Short and long help for initial dispatch in `org-working-set'; will be initialized from keymap on first call.")

(defvar org-working-set--circle-help-strings nil "Short and long help for working set circle; will be initialized from keymap on first call.")

(defvar org-working-set--menu-help-strings nil "Short and long help to be presented in working set menu; will be initialized from keymap on first call.")

(defconst org-working-set--menu-buffer-name "*working-set of org-nodes*" "Name of buffer with list of working-set nodes.")

;; Version of this package
(defvar org-working-set-version "2.3.0" "Version of `org-ẃorking-set', format is major.minor.bugfix, where \"major\" are incompatible changes and \"minor\" are new features.")


;;; The central dispatch function

(defun org-working-set ()
  ;; Do NOT edit the part of this help-text before version number. It will
  ;; be overwritten with Commentary-section from beginning of this file.
  ;; Editing after version number is fine.
  ;;
  ;; For Rake: Insert purpose here
  "Maintain a small and changing subset of your org-nodes to visit with ease.

The working-set is a small set of nodes, among which you can switch
rapidly; it is expected to change on a daily or even hourly basis.  Put
nodes into your working set in order to return easily after any
interruption.

Once you have added nodes to your working set, there are two ways to
traverse them (both are accessible through the central function
`org-working-set'): cycling through your working set is the quickest
way to return to the current node or go to others; alternatively,
invoking the working set menu allows for better control but may require
more keystrokes.

Please note, that org-working-set adds an id-property to all nodes in
the working-set.

The list of ids from the nodes of your working-set is stored within the
property-drawer of a distinguished node specified via
`org-working-set-id'; this can be any node you choose and is itself not
part of the working-set.

Remark: Depending on your needs you might also find these packages
interesting for providing somewhat similar functionality: org-now and
org-mru-clock.

This is version 2.3.0 of org-working-set.el.

The subcommands allow to:
- Modify the list of nodes (e.g. add new nodes)
- Circle quickly through the nodes
- Show a menu buffer with all nodes currently in the working set
"
  (interactive)

  (let (key def text more-text)

    (unless org-working-set--dispatch-help-strings
      (setq org-working-set--dispatch-help-strings (org-working-set--make-help-strings org-working-set-dispatch-keymap)))

    (setq org-working-set--clock-in-curr org-working-set-clock-in)
    (setq org-working-set--land-at-end-curr org-working-set-land-at-end)
    
    (unless org-working-set-id
      (org-working-set--id-assistant))
    
    (org-working-set--nodes-from-property-if-unset-or-stale)

    (while (not text)
      (setq def nil)
      (while (not def)
        (setq key (read-key-sequence
                   (concat
                    "org-working-set; type "
                    (if org-working-set--short-help-wanted
                        (cdr org-working-set--dispatch-help-strings)
                      (car org-working-set--dispatch-help-strings))
                    (org-working-set--switches-text)
                    " - ")))
        (setq def (lookup-key org-working-set-dispatch-keymap key))
        (when (or (not def)
                (numberp def))
          (message "Invalid key: %s" key)
          (setq def nil)
          (sit-for 1)))

      (setq text (funcall def)))

    (when (consp text)
      (setq more-text (cdr text))
      (setq text (car text)))

    (org-working-set--nodes-persist)

    (setq text (format text (or more-text "") (length org-working-set--ids) (if (cdr org-working-set--ids) "s" "")))
    (message (concat (upcase (substring text 0 1)) (substring text 1)))))


;;; Smaller functions directly available from dispatch; circle and menu see further down

(defun org-working-set--set ()
  "Set working-set to current node."
  (let ((id (org-id-get-create)))
    (setq org-working-set--ids-saved org-working-set--ids)
    (setq org-working-set--ids (list id))
    (setq org-working-set--id-last-goto id)
    (org-working-set--clock-in-maybe)
    "working-set has been set to current node (1 node)"))


(defun org-working-set--add ()
  "Add current node to working-set."
  (let ((more-text "")
        (id (org-id-get-create))
        name ids-up-to-top)

    (org-with-limited-levels
     (setq name (org-get-heading t t t t)))

    (unless (member id org-working-set--ids)
      (setq org-working-set--ids-saved org-working-set--ids)

      ;; before adding, remove any children of new node, that are already in working-set
      ;; i.e. remove all nodes from working set that have the new node as any of their parents
      (setq org-working-set--ids
            (delete nil (mapcar (lambda (wid)
                                  (if (member id
                                              ;; compute all parents of working set node id wid
                                              (org-with-point-at (org-id-find wid t)
                                                (org-working-set--ids-up-to-top)))
                                      ;; if new node is parent of a node already in working set
                                      (progn
                                        (setq more-text ", removing its children")
                                        nil) ; do not keep this node from working set
                                    wid)) ; keep it
                                org-working-set--ids)))

      ;; remove any parents of new node, that are already in working-set
      (setq ids-up-to-top (org-working-set--ids-up-to-top))
      (when (seq-intersection ids-up-to-top org-working-set--ids)
        (setq org-working-set--ids (seq-difference org-working-set--ids ids-up-to-top))
        (setq more-text (concat more-text ", replacing its parent")))

      ;; finally add new node to working-set
      (setq org-working-set--ids (cons id org-working-set--ids))
      (org-working-set--log-add id name))

    (setq org-working-set--id-last-goto id)
    (org-working-set--clock-in-maybe)
    (cons
     "current node has been appended to working-set%s (%d node%s)"
     more-text)))


(defun org-working-set--delete-from (&optional id)
  "Delete current node from working-set.
Optional argument ID gives the node to delete."
  (setq id (or id (org-id-get)))
  (format
   (if (and id (member id org-working-set--ids))
       (progn
         (if (string= id org-working-set--id-last-goto) (setq org-working-set--id-last-goto nil))
         (setq org-working-set--ids-saved org-working-set--ids)
         (setq org-working-set--ids (delete id org-working-set--ids))
         "Current node has been removed from working-set (%d node%s)")
     "Current node has not been in working-set (%d node%s)")
   (length org-working-set--ids) (if org-working-set--ids "s" "")))


(defun org-working-set--journal-enter ()
  "Enter journal of working set nodes and position cursor on first link."
  (org-id-goto org-working-set-id)
  (recenter 1)
  (org-end-of-meta-data t)
  (org-working-set--unfold-buffer t)
  (search-forward "[" (line-end-position) t 2)
  "log of additions to working set")


(defun org-working-set--dispatch-toggle-help ()
  "Show help."
  (interactive)
  (setq org-working-set--short-help-wanted
        (not org-working-set--short-help-wanted))
  nil)


(defun org-working-set--nodes-restore (&optional upcase)
  "Restore previously saved working-set.
Optional argument UPCASE modifies the returned message."
  (let (txt)
    (if org-working-set--ids-saved
        (progn
          (setq txt (format "Discarded current working set of and restored previous set; now %d node%s in working-set" (length org-working-set--ids-saved) (if (cdr org-working-set--ids-saved) "s" "")))
          (setq org-working-set--ids org-working-set--ids-saved))
      (setq txt "No saved working-set nodes to restore, nothing to do"))
    (if upcase (concat (upcase (substring txt 0 1))
                       (substring txt 1)
                       ".")
      txt)))


(defun org-working-set--dispatch-toggle-clock-in ()
  "Toggle between clocking in and not."
  (interactive)
  (setq org-working-set--clock-in-curr (not org-working-set--clock-in-curr))
  nil)


(defun org-working-set--dispatch-toggle-land-at-end ()
  "Toggle between landing at head or end."
  (interactive)
  (setq org-working-set--land-at-end-curr (not org-working-set--land-at-end-curr))
  nil)


;;; Functions for the working set circle

(defun org-working-set--circle-start ()
  "Go through working-set, one node after the other."
  (unless org-working-set--ids (error "No nodes in working-set; need to add some first"))

  (unless org-working-set--circle-help-strings
    (setq org-working-set--circle-help-strings (org-working-set--make-help-strings org-working-set-circle-keymap)))
    
  (setq org-working-set--short-help-wanted nil)
  (setq org-working-set--circle-before-marker (point-marker))
  (setq org-working-set--circle-win-config (current-window-configuration))

  (setq org-working-set--circle-cancel-transient-function
        (set-transient-map
         org-working-set-circle-keymap t
         ;; this is run (in any case) on leaving the map
         (lambda ()
           (if org-working-set--cancel-timer
               (cancel-timer org-working-set--cancel-timer))
           (message nil)
           (org-working-set--remove-tooltip-overlay)
           (let (keys)
             ;; save and repeat terminating key, because org-clock-in might read interactively
             (if (input-pending-p) (setq keys (read-key-sequence nil)))
             (ignore-errors (org-working-set--clock-in-maybe))
             (if keys (setq unread-command-events (listify-key-sequence keys))))
           (when org-working-set--circle-before-marker
             (move-marker org-working-set--circle-before-marker nil)
             (setq org-working-set--circle-before-marker nil)))))

  ;; first move
  (org-working-set--circle-message (org-working-set--circle-continue t)))


(defun org-working-set--circle-forward ()
  "Move forward."
    (interactive)
  (setq this-command last-command)
  (org-working-set--circle-message (org-working-set--circle-continue)))


(defun org-working-set--circle-backward ()
  "Move backward."
  (interactive)
  (setq this-command last-command)
  (org-working-set--circle-message (org-working-set--circle-continue nil t)))


(defun org-working-set--circle-toggle-clock-in ()
  "Toggle clocking."
  (interactive)
  (setq org-working-set--clock-in-curr (not org-working-set--clock-in-curr))
  (org-working-set--circle-message (org-working-set--circle-continue t)))


(defun org-working-set--circle-toggle-land-at-end ()
  "Toggle between landing at head or end."
  (interactive)
  (setq org-working-set--land-at-end-curr (not org-working-set--land-at-end-curr))
  (if org-working-set--land-at-end-curr
      (org-working-set--put-tooltip-overlay)
    (org-working-set--remove-tooltip-overlay))
  (org-working-set--circle-message (org-working-set--circle-continue t)))


(defun org-working-set--circle-done ()
  "Finish regularly."
    (interactive)
    (org-working-set--circle-message "Circle done." t)
    (org-working-set--remove-tooltip-overlay))


(defun org-working-set--circle-toggle-help ()
  "Show help."
  (interactive)
  (setq org-working-set--short-help-wanted
        (not org-working-set--short-help-wanted))
  (message (org-working-set--circle-continue t)))


(defun org-working-set--circle-delete-current ()
  "Delete current entry."
  (interactive)
  (setq this-command last-command)
  (org-working-set--nodes-persist)
  (org-working-set--circle-message (concat (org-working-set--delete-from) " "
                                    (org-working-set--circle-continue))))


(defun org-working-set--circle-quit ()
  "Leave circle and return to prior node."
  (interactive)
  (if org-working-set--circle-before-marker ; proper cleanup of marker will happen in cancel-wait timer
    (org-goto-marker-or-bmk org-working-set--circle-before-marker))
  (when org-working-set--circle-win-config
    (set-window-configuration org-working-set--circle-win-config)
    (setq org-working-set--circle-win-config nil))
  (org-working-set--remove-tooltip-overlay)
  (message "Quit")
  (if org-working-set--circle-cancel-transient-function
      (funcall org-working-set--circle-cancel-transient-function)))


(defun org-working-set--circle-continue (&optional stay back)
  "Continue with working set circle after start.
Optional argument STAY prevents changing location.
Optional argument BACK"
  (let (last-id following-id previous-id target-id parent-ids)

    ;; compute target
    (setq last-id (or org-working-set--id-last-goto
                      (car (last org-working-set--ids))))
    (setq following-id (car (or (cdr-safe (member last-id
                                                  (append org-working-set--ids org-working-set--ids)))
                                org-working-set--ids)))
    (if back
        (setq previous-id (car (or (cdr-safe (member last-id
                                                     (reverse (append org-working-set--ids org-working-set--ids))))
                                   org-working-set--ids))))
    (setq target-id (if stay last-id (if back previous-id following-id)))
    (setq parent-ids (org-working-set--ids-up-to-top)) ; remember this before changing location
    
    ;; bail out on inactivity
    (if org-working-set--cancel-timer
        (cancel-timer org-working-set--cancel-timer))
    (setq org-working-set--cancel-timer
          (run-at-time 30 nil
                       (lambda () (if org-working-set--circle-cancel-transient-function
                                 (funcall org-working-set--circle-cancel-transient-function)))))

    (org-working-set--goto-id target-id)
    (setq org-working-set--id-last-goto target-id)

    (if org-working-set--land-at-end-curr
        (org-working-set--put-tooltip-overlay))

    ;; Compose return message:
    (concat
     "In circle, "
     ;; explanation
     (format (cond (stay
                    "returning to %slast")
                   ((member target-id parent-ids)
                    "staying below %scurrent")
                   (t
                    (concat "at %s" (if back "previous" "next"))))
             (if org-working-set--land-at-end-curr "end of " ""))
     ;; count of nodes
     (if (cdr org-working-set--ids)
         (format " node (%s); type " (org-working-set--out-of-clause target-id))
       (format " single node; type "))
     ;; help text
     (if org-working-set--short-help-wanted
         (cdr org-working-set--circle-help-strings)
       (car org-working-set--circle-help-strings)))))


(defun org-working-set--circle-message (message &optional plain)
  "Issue given MESSAGE and append string '(again)' if appropriate."
  (let ((again (if (string= message org-working-set--last-message) " (again)" "")))
    (setq org-working-set--last-message message)
    (if plain
        (message message)
      (message (concat message again (org-working-set--switches-text) " - ")))))


;;; Functions for the working set menu

(defun org-working-set--menu ()
  "Show menu to let user choose among and manipulate list of working-set nodes."

  (unless org-working-set--menu-help-strings
    (setq org-working-set--menu-help-strings (org-working-set--make-help-strings org-working-set-menu-keymap)))
    
  (setq org-working-set--short-help-wanted nil)
  (pop-to-buffer org-working-set--menu-buffer-name '((display-buffer-at-bottom)))
  (org-working-set--menu-rebuild t t)

  (use-local-map org-working-set-menu-keymap)
  "Buffer with nodes of working-set")


(defun org-working-set-menu-go--this-win ()
  "Go to node specified by line under cursor in this window."
  (interactive)
  (org-working-set-menu-go nil))


(defun org-working-set-menu-go--other-win ()
  "Go to node specified by line under cursor in other window."
  (interactive)
  (org-working-set-menu-go t))


(defun org-working-set-menu-go (other-win)
  "Go to node specified by line under cursor.
The Boolean arguments OTHER-WIN goes to node in other window."
  (let (id)
    (setq id (org-working-set--menu-get-id))
    (if other-win
        (progn
          (other-window 1)
          (org-working-set--goto-id id))
      (if (> (count-windows) 1) (delete-window))
      (org-working-set--goto-id id)
      (recenter 1))

    (if org-working-set--land-at-end-curr
        (org-working-set--end-of-node))
    (setq org-working-set--id-last-goto id)
    (org-working-set--clock-in-maybe)))


(defun org-working-set--menu-peek ()
  "Peek into node specified by line under cursor."
  (interactive)
  (save-window-excursion
    (save-excursion
      (org-working-set--goto-id (org-working-set--menu-get-id))
      (delete-other-windows)
      (recenter 1)
      (read-char "Peeking into node, any key to return." nil 10))))


(defun org-working-set--menu-delete-entry ()
  "Delete node under cursor from working set."
  (interactive)
  (message (org-working-set--delete-from (org-working-set--menu-get-id)))
  (org-working-set--nodes-persist)
  (org-working-set--menu-rebuild))


(defun org-working-set--menu-undo ()
  "Undo last modification to working set."
  (interactive)
  (message (org-working-set--nodes-restore))
  (org-working-set--nodes-persist)
  (org-working-set--menu-rebuild t))


(defun org-working-set--menu-quit ()
  "Quit menu."
  (interactive)
  (delete-windows-on org-working-set--menu-buffer-name)
  (kill-buffer org-working-set--menu-buffer-name))


(defun org-working-set--menu-toggle-help ()
  "Show help."
  (interactive)
  (setq org-working-set--short-help-wanted
        (not org-working-set--short-help-wanted))
  (org-working-set--menu-rebuild t))


(defun org-working-set--menu-toggle-clock-in ()
  "Toggle between clocking in and not in working set menu."
  (interactive)
  (setq org-working-set--clock-in-curr (not org-working-set--clock-in-curr))
  (org-working-set--menu-rebuild t))


(defun org-working-set--menu-toggle-land-at-end ()
  "Toggle between landing at head or end."
  (interactive)
  (setq org-working-set--land-at-end-curr (not org-working-set--land-at-end-curr))
  (org-working-set--menu-rebuild t))


(defun org-working-set--advice-for-org-id-update-id-locations (_orig-func &rest _args)
  "Advice that moderates use of `org-id-update-id-location' for `org-working-set--menu-rebuild'."
  (org-working-set--ask-and-handle-stale-id))


(defun org-working-set--menu-rebuild (&optional resize go-top)
  "Rebuild content of menu-buffer.
Optional argument RESIZE adjusts window size.
Optional argument GO-TOP goes to top of new window, rather than keeping current position."
  (interactive)
  (let (cursor-here prev-help-len this-help-len lb)
    (org-working-set--nodes-from-property-if-unset-or-stale)
    (with-current-buffer (get-buffer-create org-working-set--menu-buffer-name)
      (set (make-local-variable 'line-move-visual) nil)
      (setq buffer-read-only nil)
      (setq cursor-here (point))
      (setq prev-help-len (next-property-change (point-min)))
      (cursor-intangible-mode)
      (erase-buffer)
      (insert (propertize (concat (if org-working-set--short-help-wanted
                                      (org-working-set--wrap (cdr org-working-set--menu-help-strings))
                                    (car org-working-set--menu-help-strings))
                                  (org-working-set--switches-text))
                          'face 'org-agenda-dimmed-todo-face
                          'cursor-intangible t
                          'front-sticky t))
      (setq this-help-len (point))
      (insert "\n\n")
      (if go-top (setq cursor-here (point)))
      (if org-working-set--ids
          (mapc (lambda (id)
                  (let (heads olpath)
                    (save-window-excursion
                      (setq org-working-set--id-not-found id)
                      ;; org-id-goto may call org-id-update-id-locations, which tends to take long
                      ;; so we advice it and ask the user if it is worthwhile
                      (unwind-protect
                          (progn (advice-add 'org-id-update-id-locations :around #'org-working-set--advice-for-org-id-update-id-locations)
                                 (org-id-goto id))
                        (advice-remove 'org-id-update-id-locations #'org-working-set--advice-for-org-id-update-id-locations))

                      (setq olpath (org-format-outline-path
                                    (reverse (org-get-outline-path)) most-positive-fixnum nil " / "))
                      (setq heads (concat (substring-no-properties (org-get-heading))
                                          (if (> (length olpath) 0)
                                              (propertize (concat " / " olpath)
                                                          'face 'org-agenda-dimmed-todo-face)
                                            ""))))
                    (insert (format "%s %s" (if (eq id org-working-set--id-last-goto) "*" " ") heads))
                    (setq lb (line-beginning-position))
                    (insert "\n")
                    (put-text-property lb (point) 'org-working-set-id id)))
                org-working-set--ids)
        (insert "  No nodes in working-set.\n"))
      (if (or go-top (not prev-help-len))
          (goto-char cursor-here)
        (goto-char (+ cursor-here (- this-help-len prev-help-len))))
      (setq org-working-set--id-not-found nil)
      (when resize
        (ignore-errors
          (fit-window-to-buffer (get-buffer-window))
          (enlarge-window 1)))
      (setq buffer-read-only t))))


(defun org-working-set--menu-get-id ()
  "Extract id from current line in working-set menu."
  (or (get-text-property (point) 'org-working-set-id)
      (error "This line does not point to a node from working-set")))


;;; General helper functions

(defun org-working-set--switches-text ()
  "Make text showing state of switches"
  (format " [clock-in: %s, land-at: %s]"
          (if org-working-set--clock-in-curr "yes" "no ")
          (if org-working-set--land-at-end-curr "end " "head")))

(defun org-working-set--goto-id (id)
  "Goto node with given ID and unfold."
  (let (marker)
    (setq org-working-set--id-not-found id)
    (unwind-protect
        (progn
          (advice-add 'org-id-update-id-locations :around #'org-working-set--advice-for-org-id-update-id-locations)
          (setq marker (org-id-find id 'marker)))
      (advice-remove 'org-id-update-id-locations #'org-working-set--advice-for-org-id-update-id-locations))
    (setq org-working-set--id-not-found nil)
    (unless marker
      (setq org-working-set--id-last-goto nil)
      (error "Could not find working-set node with id %s" id))
    
    (pop-to-buffer-same-window (marker-buffer marker))
    (goto-char (marker-position marker))
    (org-working-set--unfold-buffer)
    (move-marker marker nil)
    (when org-working-set--land-at-end-curr
      (org-working-set--end-of-node))))


(defun org-working-set--end-of-node ()
  "Goto end of current node, ignore inline-tasks but stop at first child."
  (let (level (pos (point)))
    (when (ignore-errors (org-with-limited-levels (org-back-to-heading)))
      (setq level (outline-level))
      (forward-char 1)
      (if (and (org-with-limited-levels (re-search-forward org-outline-regexp-bol nil t))
               (> (outline-level) level))
          (progn        ; landed on child node
            (goto-char (match-beginning 0))
            (forward-line -1))
        (goto-char pos) ; landed on next sibling or end of buffer
        (org-with-limited-levels
         (org-end-of-subtree nil t)
         (when (org-at-heading-p)
           (forward-line -1))))
      (beginning-of-line)
      (org-reveal))
    (recenter -2)))


(defun org-working-set--nodes-persist ()
  "Write working-set to property."
  (let ((bp (org-working-set--id-bp)))
    (with-current-buffer (car bp)
      (setq org-working-set--ids (cl-remove-duplicates org-working-set--ids :test (lambda (x y) (string= x y))))
      (org-entry-put (cdr bp) "working-set-nodes" (mapconcat #'identity org-working-set--ids " ")))))


(defun org-working-set--nodes-from-property-if-unset-or-stale ()
  "Read working-set to property if conditions apply."
    (if (or (not org-working-set--ids)
            org-working-set--id-not-found)
        (let ((bp (org-working-set--id-bp)))
          (with-current-buffer (car bp)
            (save-excursion
              (goto-char (cdr bp))
              (setq org-working-set--ids (split-string (or (org-entry-get nil "working-set-nodes") "")))
              (when (member org-working-set--id-not-found org-working-set--ids)
                (org-working-set--ask-and-handle-stale-id)))))
      (setq org-working-set--id-not-found nil)))


(defun org-working-set--ask-and-handle-stale-id ()
  "Ask user about stale ID from working set and handle answer."
  (let ((char-choices (list ?d ?u ?q))
        (window-config (current-window-configuration))
        char)

    (org-working-set--show-explanation
     "*ID not found*"
     (format "ERROR: ID %s from working set cannot be found. Please specify how to proceed:\n" org-working-set--id-not-found)
     "  - d :: delete this ID from the working set"
     "  - u :: run `org-id-update-id-locations' to rescan your org-files"
     "  - q :: quit and do nothing"
     "\nIf unsure, try 'u' first and then 'd'."
     "In any case the current function will be aborted and you will need to start over.")
    (unwind-protect
        (while (not (memq char char-choices))
          (setq char (read-char-choice "Your choice: " char-choices)))
      (kill-buffer-and-window)
      (set-window-configuration window-config))

    (cond
     ((eq char ?q)
      (keyboard-quit))
     ((eq char ?d)
      (setq org-working-set--ids-saved org-working-set--ids)
      (setq org-working-set--ids (delete org-working-set--id-not-found org-working-set--ids))
      (org-working-set--nodes-persist)
      (setq org-working-set--id-not-found nil)
      (error "Removed ID %s from working-set; please start over" org-working-set--id-not-found))
     ((eq char ?u)
      (message "Updating ID locations")
      (sit-for 1)
      (org-id-update-id-locations)
      (error "Searched all files for ID %s; please start over" org-working-set--id-not-found)))))


(defun org-working-set--clock-in-maybe ()
  "Clock into current node if appropriate."
  (if org-working-set--clock-in-curr
      (org-with-limited-levels (org-clock-in))))


(defun org-working-set--wrap (text)
     "Wrap TEXT at fill column."
     (with-temp-buffer
       (insert text)
       (fill-region (point-min) (point-max) nil t)
       (buffer-string)))


(defun org-working-set--unfold-buffer (&optional skip-recenter)
  "Helper function to unfold buffer.
Optional argument SKIP-RECENTER avoids recentering of buffer in window."
  (org-show-context 'tree)
  (org-reveal '(16))
  (unless skip-recenter (recenter 1)))


(defun org-working-set--id-bp ()
  "Return buffer of working-set node."
  (let (fp)
    (setq fp (org-id-find org-working-set-id))
    (unless fp (error "Could not find node %s" org-working-set-id))
    (cons (get-file-buffer (car fp))
          (cdr fp))))


(defun org-working-set--show-explanation (buffer-name &rest strings)
  "Show buffer BUFFER-NAME with explanations STRINGS."
  (pop-to-buffer buffer-name '((display-buffer-at-bottom)) nil)
  (with-current-buffer buffer-name
    (erase-buffer)
    (org-mode)
    (mapc
     (lambda (x) (insert x) (org-fill-paragraph) (insert "\n"))
     strings)
    (setq mode-line-format nil)
    (setq buffer-read-only t)
    (setq cursor-type nil)
    (fit-window-to-buffer)
    (enlarge-window 1)
    (goto-char (point-min))
    (recenter 0)
    (setq window-size-fixed 'height)))


(defun org-working-set--id-assistant ()
  "Assist the user in choosing a node, where the list of working-set nodes can be stored."
  (let ((window-config (current-window-configuration))
        (current-heading (ignore-errors (org-get-heading)))
        use-current-node)

    (org-working-set--show-explanation
     "*org working-set assistant*"
     "\nThe required variable `org-working-set-id' has not been set. It should contain the id of an empty node, where org-working-set will store its runtime information. The property drawer will be used to store the ids of the working-set nodes, the body will be populated with an ever-growing list of nodes, that have been added."
     "\nThere are three ways to set `org-working-set-id':"
     "- Choose a node and copy the value of its ID-property; use the customize-interface to set `org-working-set-id' to the chosen id."
     "- As above, but edit your .emacs and insert a setq-clause."
     (format "- Use the ID of the node the cursor is currently positioned in (which is '%s')." current-heading)
     "\nIf you choose the first or second way, you should answer 'no' to the question below and go ahead yourself."
     "\nIf you choose the third way, you should answer 'yes'."
     (format "\nHowever, if you are not already within the right node, you may answer 'no' to the question below, navigate to the right node and invoke `%s' again." this-command))
    (unwind-protect
        (setq use-current-node (yes-or-no-p "Do you want to use the id of the current node ? "))
      (kill-buffer-and-window)
      (set-window-configuration window-config))


    (if use-current-node
        (let ((id (org-id-get-create)))
          (customize-save-variable 'org-working-set-id id)
          (message "Using id of current node to store `org-working-set-id'")
          (sit-for 1))
      (error "`org-working-set-id' not set"))))


(defun org-working-set--make-help-strings (keymap)
  "Construct short and long help strings for given keymap."
  (let (direct-keys grouped short long)
    (setq direct-keys ; ((function1 . key1) (function1 . key2) (function2 . key3) ...)
          (reverse
           (mapcar (lambda (cell) (cons (cdr cell) (car cell))) ; swap car and cdr
                   (-take-while (lambda (x) (consp x)) ; ignore keys from parent keymaps, after next symbol 'keymap
                                (cdr (car (list keymap))))))) ; direct key-definitionss come after an initial symbol 'keymap
    (setq short (concat
                 (s-join ","
                         (-remove (lambda (x) (member x '("?" "C-g" "q" "<tab>")))
                                  (mapcar
                                   (lambda (def-key) (single-key-description (cdr def-key)))
                                   direct-keys)))
                 " or ? for short help"))
    (setq grouped (-group-by 'car direct-keys)) ; ((function1 key1 key2) (function2 key3 key4) ...)
    (setq long
          (mapconcat (lambda (group)
                       (concat
                        (s-join "," 
                                (-remove (lambda (x) (member x '("<tab>")))
                                         (mapcar 
                                          (lambda (kcell) (single-key-description (cdr kcell)))
                                          (cdr group))))
                        ") "
                        (s-chop-suffix "." (cl-first (s-lines (documentation (car group)))))))
                     grouped
                     ",  "))
    (cons short long)))


(defun org-working-set--ids-up-to-top ()
  "Get list of all ids from current node up to top level."
  (when (string= major-mode "org-mode")
    (let (ids id pt)
      (save-excursion
        (ignore-errors
          (while (progn (and (setq id (org-id-get))
                             (setq ids (cons id ids)))
                        (setq pt (point))
                        (outline-up-heading 1)
                        (/= pt (point))))))
      ids)))


(defun org-working-set--log-add (id title)
  "Add entry into log of working-set nodes.
ID and TITLE specify heading to log"
  (let ((bp (org-working-set--id-bp)))
    (with-current-buffer (car bp)
      (save-excursion
        (goto-char (cdr bp))
        (org-end-of-meta-data t)  ; skips over empty lines too
        (when (org-at-heading-p)  ; no log-line yet
	  (backward-char)         ; needed for tests to work around an edge-case in save-excursion
          (insert "\n\n\n")
          (forward-line -1))
	(insert (make-string (1+ (org-current-level)) ? )
		"- ")
        (org-insert-time-stamp nil t t)
        (insert (format "    [[id:%s][%s]]\n" id title))))))


(defun org-working-set--put-tooltip-overlay ()
  "Create and show overlay for tooltip."
  (let (head)
    (setq head (org-with-limited-levels (org-get-heading t t t t)))
    (when org-working-set--land-at-end-curr
      (if org-working-set--overlay (delete-overlay org-working-set--overlay))
      (setq org-working-set--overlay (make-overlay (point-at-bol) (point-at-bol)))
      (overlay-put org-working-set--overlay
                   'after-string
                   (propertize
                    (format " %s (%s) " head (org-working-set--out-of-clause (org-id-get)))
                    'face 'match))
      (overlay-put org-working-set--overlay 'priority most-positive-fixnum))))


(defun org-working-set--out-of-clause (id)
  "Create string describing position in working set."
  (format "%d of %d"
          (1+ (- (length org-working-set--ids)
                 (length (member id org-working-set--ids))))
          (length org-working-set--ids)))


(defun org-working-set--remove-tooltip-overlay ()
  "Remove overlay for tooltip"
  (if org-working-set--overlay
      (delete-overlay org-working-set--overlay))
  (setq org-working-set--overlay nil))


(provide 'org-working-set)

;; Local Variables:
;; fill-column: 95
;; comment-column: 50
;; End:

;;; org-working-set.el ends here
