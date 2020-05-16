;;; org-working-set.el --- Manage a working-set of org-nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Marc Ihm <1@2484.de>
;; URL: https://github.com/marcIhm/org-working-set
;; Version: 2.2.0
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

(defvar org-working-set--ids nil "Ids of working-set nodes (if any).")
(defvar org-working-set--ids-saved nil "Backup for ‘org-working-set--ids’.")
(defvar org-working-set--id-last-goto nil "Id of last node from working-set, that has been visited.")
(defvar org-working-set--circle-before-marker nil "Marker for position before entry into circle.")
(defvar org-working-set--circle-win-config nil "Window configuration before entry into circle.")
(defvar org-working-set--last-message nil "Last message issued by working-set commands.")
(defvar org-working-set--cancel-wait-function nil "Function to call on timeout for working-set commands.")
(defvar org-working-set--cancel-timer nil "Timer to cancel waiting for key.")
(defvar org-working-set--overlay nil "Overlay to display name of current working-set node.")
(defvar org-working-set--short-help-wanted nil "Non-nil, if short help should be displayed in working-set menu.")
(defvar org-working-set--id-not-found nil "Id of last node not found.")

(defun org-working-set--define-keymap (keymap keylist)
  "Define Keys given by KEYLIST in KEYMAP."
  (dolist (keyentry keylist)
    (dolist (key (car keyentry))
      (define-key keymap (kbd key) (cdr keyentry))))
  keymap)

(defvar org-working-set-circle-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap org-mode-map)
    (org-working-set--define-keymap
     keymap
     '((("c" "SPC") . org-working-set-circle-forward)
       (("RET" "q") . org-working-set-circle-done)
       (("DEL" "<backspace>") . org-working-set-circle-backward)
       (("w" "m") . org-working-set-circle-switch-to-menu)
       (("h") . org-working-set-circle-head-of-node)
       (("b") . org-working-set-circle-bottom-of-node)
       (("?") . org-working-set-circle-show-help)
       (("d") . org-working-set-circle-delete-current)
       (("C-g") . org-working-set-circle-quit))))
  "Keymap used in working set circle.")

(defvar org-working-set-circle-help-strings
  '("; type c,space,d,q,ret,,bs,m,w or ? for short help" .
"; type 'c' or space to jump to next node in circle; type 'd' to delete this node from list; 'q',<return> accepts current position, <backspace> proceeds in reverse order, 'm' or 'w' switch to working set menu, C-g returns to initial position")
  "Short and long help to be presented in working set circle.")

(defvar org-working-set-menu-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap org-mode-map)
    (org-working-set--define-keymap
     keymap
     '((("<return>" "RET") . org-working-set-menu-go--this-win)
       (("<tab>") . org-working-set-menu-go--other-win)
       (("p") . org-working-set-menu-peek)
       (("d") . org-working-set-menu-delete-entry)
       (("u") . org-working-set-menu-undo)
       (("q") . org-working-set-menu-quit)
       (("?") . org-working-set-menu-help)
       (("r") . org-working-set-menu-rebuild))))
  "Keymap used in working set menu.")

(defvar org-working-set-menu-help-strings
  '("Press <return>,<tab>,h,H,b,B,p,d,u,q,r,c,~,* or ? to toggle short help." .
    "List of working-set nodes. Pressing <return> on a list element jumps to node in other window and deletes this window, <tab> does the same but keeps this window, `h' and `b' jump to bottom of node unconditionally (with capital letter in other windows), `p' peeks into node from current line, `d' deletes node from working-set immediately, `u' undoes last delete, `q' aborts and deletes this buffer, `r' rebuilds its content, `c' toggles clocking. Markers on nodes are: `*' for last visited.")
  "Short and long help to be presented in working set menu.")

(defvar org-working-set--menu-keymap nil "Keymap used in working set menu.")

(defconst org-working-set--menu-buffer-name "*working-set of org-nodes*" "Name of buffer with list of working-set nodes.")

;; Version of this package
(defvar org-working-set-version "2.2.0" "Version of `org-ẃorking-set', format is major.minor.bugfix, where \"major\" are incompatible changes and \"minor\" are new features.")

;; customizable options
(defgroup org-working-set nil
  "Options concerning the optional working-set of org-nodes."
  :tag "Org Working-set"
  :group 'org)

(defcustom org-working-set-id nil
  "Id of the Org-mode node, which contains the index table.  This should be set to the id of an empty node. The property drawer will be used to store the ids of the working-set nodes, the body will be populated with an ever-growing list of nodes, that have been added."
  :type 'string
  :group 'org-working-set)

(defcustom org-working-set-clock-in nil
  "Clock into nodes of working-set ?"
  :group 'org-working-set
  :type 'boolean)

(defcustom org-working-set-show-overlay t
  "Show overlay text when traversing the working-set."
  :group 'org-working-set
  :type 'boolean)

(defcustom org-working-set-goto-bottom nil
  "After visiting a node from the working-set; position cursor at bottom of node (as opposed to heading) ?"
  :group 'org-working-set
  :type 'boolean)


(defun org-working-set (&optional silent)
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

This is version 2.2.0 of org-working-set.el.

The subcommands allow to:
- Modify the list of nodes (e.g. add new nodes)
- Circle quickly through the nodes
- Show a menu buffer with all nodes currently in the working set

Optional argument SILENT does not issue final message."
  (interactive)

  (unless org-working-set-id
    (org-working-set--id-assistant))
  
  (org-working-set--nodes-from-property-if-unset-or-stale)
  
  (let ((char-choices (list ?s ?S ?a ?A ?d ?u ?l ?w ?m ?c ?g ? ??))
        id name text more-text char prompt ids-up-to-top)

    (setq prompt (format "Please specify action on working-set of %d nodes (s,S,a,A,d,u,l,m,w,c,space,g or ? for short help) - " (length org-working-set--ids)))
    (while (or (not (memq char char-choices))
               (= char ??))
      (setq char (read-char-choice prompt char-choices))
      (setq prompt (format "Actions on working-set of %d nodes:  s)et working-set to this node alone, a)ppend this node to set,  d)elete this node from list,  u)ndo last modification of working set,  l) shows log of entries added,  w),m)enu to edit working set,  c),space) enter working set circle,  g)o to bottom position in current node.  Please choose - " (length org-working-set--ids))))

    (when (and (memq char (list ?s ?S ?a ?A ?d))
               (not (string= major-mode "org-mode")))
      (error "Current buffer is not in org-mode"))

    (setq text
          (cond

           ((or (eq char ?s))
            (org-working-set--set))

           ((or (eq char ?a)
                (eq char ?A))
            (org-working-set--add))

           ((eq char ?d)
            (org-working-set--delete-from))

           ((memq char '(?m ?w))
            (org-working-set--menu))

           ((memq char '(?c ? ))
            (org-working-set--circle-start))

           ((eq char ?g)
            (org-working-set--bottom-of-node)
            "at bottom of node")

           ((eq char ?l)
            (org-working-set--log-enter))

           ((eq char ?u)
            (org-working-set--nodes-restore))))

    (when (consp text)
      (setq more-text (cdr text))
      (setq text (car text)))

    (org-working-set--nodes-persist)
    
    (setq text (format text (or more-text "") (length org-working-set--ids) (if (cdr org-working-set--ids) "s" "")))
    (unless silent (message (concat (upcase (substring text 0 1)) (substring text 1))))
    text))


(defun org-working-set--log-add (id name)
  "Add entry into log of working-set nodes."
  (let ((bp (org-working-set--id-bp)))
    (set-buffer (car bp))
    (save-excursion
      (goto-char (cdr bp))
      (org-end-of-meta-data t)
      (when (org-at-heading-p)
	(backward-char) ; needed to make save-excursion work right
        (insert "\n\n")
        (forward-line -1))
      (if (looking-at "^[[:blank:]]*$")
          (forward-line))
      (insert "\n")
      (forward-line -1)
      (org-indent-line) ; works best on empty line
      (insert "- ")
      (org-insert-time-stamp nil t t)
      (insert (format "    [[id:%s][%s]]" id name)))))


(defun org-working-set--log-enter ()
  "Enter log of working set nodes and position cursor on first link."
  (org-id-goto org-working-set-id)
  (recenter 1)
  (org-end-of-meta-data t)
  (org-working-set--unfold-buffer t)
  (search-forward "[" (line-end-position) t 2)
  "log of additions to working set")


(defun org-working-set--circle-start ()
  "Go through working-set, one node after the other."
  (unless org-working-set--ids (error "No nodes in working-set; need to add some first"))

  (setq org-working-set--short-help-wanted nil)
  (setq org-working-set--circle-before-marker (point-marker))
  (setq org-working-set--circle-win-config (current-window-configuration))

  (setq org-working-set--cancel-wait-function
        (set-transient-map
         org-working-set-circle-keymap t
         ;; this is run (in any case) on leaving the map
         (lambda () (cancel-timer org-working-set--cancel-timer)
           (message nil)
           ;; Clean up overlay
           (if org-working-set--overlay (delete-overlay org-working-set--overlay))
           (setq org-working-set--overlay nil)
           (let (keys)
             ;; save and repeat terminating key, because org-clock-in might read interactively
             (if (input-pending-p) (setq keys (read-key-sequence nil)))
             (ignore-errors (org-working-set--clock-in-maybe))
             (if keys (setq unread-command-events (listify-key-sequence keys))))
           (if org-working-set--circle-before-marker (move-marker org-working-set--circle-before-marker nil)))))

  ;; first move
  (org-working-set--message (org-working-set--circle-continue t)))


(defun org-working-set-circle-forward ()
  "Move forward in working set circle."
    (interactive)
  (setq this-command last-command)
  (org-working-set--message (org-working-set--circle-continue)))


(defun org-working-set-circle-backward ()
  "Move backward in working set circle."
  (interactive)
  (setq this-command last-command)
  (org-working-set--message (org-working-set--circle-continue nil t)))


(defun org-working-set-circle-switch-to-menu ()
  "Leave working set circle and enter menu."
    (interactive)
  (org-working-set--message "Switching to menu")
  (org-working-set--circle-finished-helper t)
  (run-with-timer 0 nil 'org-working-set--menu))


(defun org-working-set-circle-done ()
  "Finish working set circle regularly."
    (interactive)
  (org-working-set--message "Circle done")
  (org-working-set--circle-finished-helper))


(defun org-working-set-circle-head-of-node ()
  "Go to head of node after leaving working set circle."
  (interactive)
  (org-working-set--head-of-node)
  (org-working-set--message "On heading of node from working-set"))


(defun org-working-set-circle-bottom-of-node ()
  "Go to head of node after leaving working set circle."
  (interactive)
  (org-working-set--bottom-of-node)
  (org-working-set--message "At bottom of node from working-set"))


(defun org-working-set-circle-show-help ()
  "Show help within working set circle."
  (interactive)
  (setq org-working-set--short-help-wanted t)
  (message (org-working-set--circle-continue t))
  (setq org-working-set--cancel-wait-function nil)
  (setq org-working-set--short-help-wanted nil))


(defun org-working-set-circle-delete-current ()
  "Delete current entry from working set circle."
  (interactive)
  (setq this-command last-command)
  (org-working-set--nodes-persist)
  (org-working-set--message (concat (org-working-set--delete-from) " "
                                    (org-working-set--circle-continue)))
  (setq org-working-set--cancel-wait-function nil))


(defun org-working-set-circle-quit ()
  "Leave working set circle and return to prior node."
  (interactive)
  (if org-working-set--circle-before-marker
      (org-goto-marker-or-bmk org-working-set--circle-before-marker))
  (if org-working-set--circle-win-config
      (set-window-configuration org-working-set--circle-win-config))
  (message "Quit")
  (org-working-set--circle-finished-helper))


(defun org-working-set--circle-finished-helper ()
  "Common steps on finishing of working set circle."
  (if org-working-set--overlay (delete-overlay org-working-set--overlay))
  (setq org-working-set--overlay nil)
  (setq org-working-set--cancel-wait-function nil))


(defun org-working-set--circle-continue (&optional stay back)
  "Continue with working set circle after start.
Optional argument STAY prevents changing location.
Optional argument BACK"
  (let (last-id following-id previous-id target-id parent-ids head)

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
    (if org-working-set--cancel-timer (cancel-timer org-working-set--cancel-timer))
    (setq org-working-set--cancel-timer
          (run-at-time 30 nil
                       (lambda () (if org-working-set--cancel-wait-function
                                 (funcall org-working-set--cancel-wait-function)))))

    (org-working-set--goto-id target-id)
    (setq org-working-set--id-last-goto target-id)

    ;; tooltip-overlay to show current heading
    (setq head (org-with-limited-levels (org-get-heading t t t t)))
    (when org-working-set-show-overlay
      (if org-working-set--overlay (delete-overlay org-working-set--overlay))
      (setq org-working-set--overlay (make-overlay (point-at-eol) (point-at-eol)))
      (overlay-put org-working-set--overlay
                   'after-string
                   (propertize
                    (format " %s (%d of %d) "
                            head
                            (1+ (- (length org-working-set--ids)
                                   (length (member target-id org-working-set--ids))))
                            (length org-working-set--ids))
                    'face 'match))
      (overlay-put org-working-set--overlay 'priority most-positive-fixnum))

    ;; Compose return message:
    (concat
     ;; title of node
     (format "Node %s, " (propertize head 'face 'org-todo))
     ;; explanation
     (format (cond (stay
                    "returning to %slast")
                   ((member target-id parent-ids)
                    "staying below %scurrent")
                   (t
                    (concat "at %s" (if back "previous" "next"))))
             (if org-working-set-goto-bottom "bottom of " ""))
     ;; count of nodes
     (if (cdr org-working-set--ids)
         (format " node (out of %d)" (length org-working-set--ids))
       (format " single node"))
     ;; help text
     (if org-working-set--short-help-wanted
         (cdr org-working-set-circle-help-strings)
       (car org-working-set-circle-help-strings)))))


(defun org-working-set--menu ()
  "Show menu to let user choose among and manipulate list of working-set nodes."

  (setq  org-working-set--short-help-wanted nil)
  (pop-to-buffer org-working-set--menu-buffer-name '((display-buffer-at-bottom)))
  (org-working-set-menu-rebuild t t)

  (use-local-map org-working-set-menu-keymap)
  "Buffer with nodes of working-set")

;;
;; A series of similar functions to be used in org-working-set-menu-keymap
;;
(defun org-working-set-menu-go--this-win ()
  "Go to node specified by line under cursor; variants: go in this win, go to default location."
  (interactive) (org-working-set-menu-go nil))


(defun org-working-set-menu-go--other-win ()
  "Go to node specified by line under cursor; variants: go in other win, go to default location."
  (interactive) (org-working-set-menu-go t))


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

    (if org-working-set-goto-bottom
        (org-working-set--bottom-of-node))
    (setq org-working-set--id-last-goto id)
    (org-working-set--clock-in-maybe)))


(defun org-working-set-menu-peek ()
  "Peek into node specified by line under cursor."
  (interactive)
  (save-window-excursion
    (save-excursion
      (org-working-set--goto-id (org-working-set--menu-get-id))
      (delete-other-windows)
      (recenter 1)
      (read-char "Peeking into node, any key to return." nil 10))))


(defun org-working-set-menu-delete-entry ()
  "Delete node under cursor from working set."
  (interactive)
  (message (org-working-set--delete-from (org-working-set--menu-get-id)))
  (org-working-set--nodes-persist)
  (org-working-set-menu-rebuild))


(defun org-working-set-menu-undo ()
  "Undo last modification to working set."
  (interactive)
  (message (org-working-set--nodes-restore))
  (org-working-set--nodes-persist)
  (org-working-set-menu-rebuild t))


(defun org-working-set-menu-quit ()
  "Quit working set menu."
  (interactive)
  (delete-windows-on org-working-set--menu-buffer-name)
  (kill-buffer org-working-set--menu-buffer-name))


(defun org-working-set-menu-help ()
  "Toggle between long and short help in working set menu."
  (interactive)
  (setq org-working-set--short-help-wanted (not org-working-set--short-help-wanted))
  (org-working-set-menu-rebuild t))


(defun org-working-set--advice-for-org-id-update-id-locations (orig-func &rest args)
  "Advice that moderates use of `org-id-update-id-location' for `org-working-set-menu-rebuild'"
  (org-working-set--ask-and-handle-stale-id))


(defun org-working-set-menu-rebuild (&optional resize go-top)
  "Rebuild content of working-set menu-buffer.
Optional argument RESIZE adjusts window size."
  (interactive)
  (let (cursor-here lb)
    (org-working-set--nodes-from-property-if-unset-or-stale)
    (with-current-buffer (get-buffer-create org-working-set--menu-buffer-name)
      (set (make-local-variable 'line-move-visual) nil)
      (setq buffer-read-only nil)
      (setq cursor-here (point))
      (cursor-intangible-mode)
      (erase-buffer)
      (insert (propertize (if org-working-set--short-help-wanted
                              (org-working-set--wrap (cdr org-working-set-menu-help-strings))
                            (car org-working-set-menu-help-strings))
                          'face 'org-agenda-dimmed-todo-face
                          'cursor-intangible t
                          'front-sticky t))
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
      (goto-char cursor-here)
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
    (when org-working-set-goto-bottom
      (org-working-set--bottom-of-node))))


(defun org-working-set--message (message)
  "Issue given MESSAGE and append string '(again)' if appropriate."
  (let ((again (if (string= message org-working-set--last-message) " (again)" "")))
    (setq org-working-set--last-message message)
    (message (concat message again "."))))


(defun org-working-set--bottom-of-node ()
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


(defun org-working-set--head-of-node ()
  "Goto head of current node."
  (org-with-limited-levels (org-back-to-heading))
  (recenter 2))


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
        char prompt)

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
        (id (org-id-get-create)))

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


(defun org-working-set--clock-in-maybe ()
  "Clock into current node if appropriate."
  (if org-working-set-clock-in
      (org-with-limited-levels (org-clock-in))))


(defun org-working-set--wrap (text)
     "Wrap TEXT at fill column."
     (with-temp-buffer
       (insert text)
       (fill-region (point-min) (point-max) nil t)
       (buffer-string)))


(defun org-working-set--unfold-buffer (&optional skip-recenter)
  "Helper function to unfold buffer."
  (org-show-context 'tree)
  (org-reveal '(16))
  (unless skip-recenter (recenter 1)))


(defun org-working-set--id-bp ()
  "Return buffer of working-set node."
  (let (marker ret)
    (setq marker (org-id-find org-working-set-id 'marker))
    (unless marker (error "Could not find node %s" org-working-set-id))
    (setq ret (cons (marker-buffer marker)
                    (marker-position marker)))
    (move-marker marker nil)
    ret))


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


(provide 'org-working-set)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; org-working-set.el ends here
