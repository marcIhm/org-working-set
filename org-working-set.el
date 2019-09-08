;;; org-working-set.el --- Manage a working-set of org-nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2019 Free Software Foundation, Inc.

;; Author: Marc Ihm <1@2484.de>
;; URL: https://github.com/marcIhm/org-working-set
;; Version: 1.0.3
;; Package-Requires: ((emacs "24.4"))

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
;;  - Invoke `M-x org-customize', group 'Org Working-set', and
;;    set `org-working-set-id'.
;;

;;; Change Log:

;;   Version 1.0.0
;;
;;   - Moved functions for working set into its own file
;;   - Show breadcrumbs in working-set-menu
;;   - Prepare for melpa
;;
;;  See the package org-index for older news

;;; Code:

(require 'org)

(defvar org-working-set--ids nil "Ids of working-set nodes (if any).")
(defvar org-working-set--ids-do-not-clock nil "Subset of `org-working-set--ids', that are not clocked.")
(defvar org-working-set--ids-saved nil "Backup for ‘org-working-set--ids’.")
(defvar org-working-set--id-last-goto nil "Id of last node from working-set, that has been visited.")
(defvar org-working-set--circle-before-marker nil "Marker for position before entry into circle.")
(defvar org-working-set--circle-win-config nil "Window configuration before entry into circle.")
(defvar org-working-set--last-message nil "Last message issued by working-set commands.")
(defvar org-working-set--circle-bail-out nil "Set, if bailing out of working-set circle.")
(defvar org-working-set--cancel-wait-function nil "Function to call on timeout for working-set commands.")
(defvar org-working-set--cancel-timer nil "Timer to cancel waiting for key.")
(defvar org-working-set--overlay nil "Overlay to display name of current working-set node.")
(defvar org-working-set--short-help-wanted nil "Non-nil, if short help should be displayed in working-set menu.")

(defconst org-working-set--menu-buffer-name "*working-set of org-nodes*" "Name of buffer with list of working-set nodes.")

;; Version of this package
(defvar org-working-set-version "1.0.3" "Version of `org-ẃorking-set', format is major.minor.bugfix, where \"major\" are incompatible changes and \"minor\" are new features.")

;; customizable options
(defgroup org-working-set nil
  "Options concerning the optional working-set of org-nodes."
  :tag "Org Working-set"
  :group 'org)

(defcustom org-working-set-id nil
  "Id of the Org-mode node, which contains the index table.  This can be set to the id of any node you like; only its property drawer will be used."
  :type 'string
  :group 'org-working-set)

(defcustom org-working-set-clock-into-working-set nil
  "Clock into nodes of working-set ?"
  :group 'org-working-set
  :type 'boolean)

(defcustom org-working-set-show-working-set-overlay t
  "Show overlay text when traversing the working-set."
  :group 'org-working-set
  :type 'boolean)

(defcustom org-working-set-goto-bottom-in-working-set nil
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

Please note, that org-working-set adds an id-property to all nodes in
the working-set.

The list of ids from the nodes of your working-set is stored within the
property-drawer of a distinguished node specified via
`org-working-set-id'; this can be any node you choose and is itself not
part of the working-set.

Remark: Depending on your needs you might also find these packages
interesting for providing somewhat similar functionality: org-now and
org-mru-clock.

This is version 1.0.3 of org-working-set.el.

The subcommands allow to:
- Modify the list of nodes (e.g. add new nodes)
- Circle quickly through the nodes
- Show a menu buffer with all nodes currently in the working set

Optional argument SILENT does not issue final message."
  (interactive)

  (unless org-working-set-id
    (org-working-set--set-id-assistant))
  
  (unless org-working-set--ids
    (let ((bp (org-working-set--id-bp)))
      (with-current-buffer (car bp)
        (save-excursion
          (goto-char (cdr bp))
          (setq org-working-set--ids (split-string (or (org-entry-get nil "working-set-nodes") "")))
          (setq org-working-set--ids-do-not-clock (split-string (or (org-entry-get nil "working-set-nodes-do-not-clock") "")))))))
  
  (let ((char-choices (list ?s ?S ?a ?A ?d ?u ?w ?m ?c ?g ? ??))
        id text more-text char prompt ids-up-to-top)

    (setq prompt (format "Please specify action on working-set of %d nodes (s,S,a,A,d,u,m,w,c,space,g or ? for short help) - " (length org-working-set--ids)))
    (while (or (not (memq char char-choices))
               (= char ??))
      (setq char (read-char-choice prompt char-choices))
      (setq prompt (format "Actions on working-set of %d nodes:  s)et working-set to this node alone,  S)et but do not clock,  a)ppend this node to set,  A)ppend but do not clock,  d)elete this node from list,  u)ndo last modification of working set,  w),m)enu to edit working set (same as 'w'),  c),space) enter working set circle,  g)o to bottom position in current node.  Please choose - " (length org-working-set--ids))))

    (when (and (memq char (list ?s ?S ?a ?A ?d))
               (not (string= major-mode "org-mode")))
      (error "Current buffer is not in org-mode"))

    (setq text
          (cond

           ((or (eq char ?s)
                (eq char ?S))
            (setq id (org-id-get-create))
            (setq org-working-set--ids-saved org-working-set--ids)
            (setq org-working-set--ids (list id))
            (if (eq char ?S)
                (setq org-working-set--ids-do-not-clock (list id))
              (setq org-working-set--id-last-goto id)
              (if org-working-set-clock-into-working-set (org-with-limited-levels (org-clock-in))))
            "working-set has been set to current node (1 node)")

           ((or (eq char ?a)
                (eq char ?A))
            (setq id (org-id-get-create))
            (unless (member id org-working-set--ids)
              ;; remove any children, that are already in working-set
              (setq org-working-set--ids
                    (delete nil (mapcar (lambda (x)
                                          (if (member id (org-with-point-at (org-id-find x t)
                                                           (org-working-set--ids-up-to-top)))
                                              (progn
                                                (setq more-text ", removing its children")
                                                nil)
                                            x))
                                        org-working-set--ids)))
              (setq org-working-set--ids-saved org-working-set--ids)
              ;; remove parent, if already in working-set
              (setq ids-up-to-top (org-working-set--ids-up-to-top))
              (when (seq-intersection ids-up-to-top org-working-set--ids)
                (setq org-working-set--ids (seq-difference org-working-set--ids ids-up-to-top))
                (setq more-text (concat more-text ", replacing its parent")))
              (setq org-working-set--ids (cons id org-working-set--ids)))
            (if (eq char ?A)
                (setq org-working-set--ids-do-not-clock (cons id org-working-set--ids-do-not-clock))
              (setq org-working-set--id-last-goto id)
              (if org-working-set-clock-into-working-set (org-with-limited-levels (org-clock-in))))
            "current node has been appended to working-set%s (%d node%s)")

           ((eq char ?d)
            (org-working-set--delete-from))

           ((memq char '(?m ?w))
            (org-working-set--menu))

           ((memq char '(?c ? ))
            (org-working-set--circle-start))

           ((eq char ?g)
            (org-working-set--bottom-of-node)
            "at bottom of node")

           ((eq char ?u)
            (org-working-set--nodes-restore))))

    (org-working-set--nodes-persist)
    
    (setq text (format text (or more-text "") (length org-working-set--ids) (if (cdr org-working-set--ids) "s" "")))
    (unless silent (message (concat (upcase (substring text 0 1)) (substring text 1))))
    text))


(defun org-working-set--circle-start ()
  "Go through working-set, one node after the other."
  (unless org-working-set--ids (error "No nodes in working-set; need to add some first"))

  (setq org-working-set--short-help-wanted nil)
  (setq org-working-set--circle-before-marker (point-marker))
  (setq org-working-set--circle-win-config (current-window-configuration))

  (let ((kmap (make-sparse-keymap)))
    (mapc (lambda (x)
            (define-key kmap (vector x)
              (lambda () (interactive)
                (setq this-command last-command)
                (org-working-set--message (org-working-set--circle-continue)))))
          (list ?c ? ))
    (mapc (lambda (x)
            (define-key kmap (kbd x)
              (lambda () (interactive)
                (org-working-set--message "Circle done")
                (org-working-set--circle-finished-helper nil))))
          (list "RET" "<return>" "q"))
    (mapc (lambda (x)
            (define-key kmap (kbd x)
              (lambda () (interactive)
                (setq this-command last-command)
                (org-working-set--message (org-working-set--circle-continue nil t)))))
          (list "DEL" "<backspace>"))
    (mapc (lambda (x)
            (define-key kmap (kbd x)
              (lambda () (interactive)
                (org-working-set--message "Switching to menu")
                (org-working-set--circle-finished-helper t)
                (run-with-timer 0 nil 'org-working-set--menu))))
          (list "w" "m"))
    (define-key kmap (vector ?h)
      (lambda () (interactive)
        (org-working-set--head-of-node)
        (org-working-set--message "On heading of node from working-set")))
    (define-key kmap (vector ?b)
      (lambda () (interactive)
        (org-working-set--bottom-of-node)
        (org-working-set--message "At bottom of node from working-set")))
    (define-key kmap (vector ??)
      (lambda () (interactive)
        (setq org-working-set--short-help-wanted t)
        (message (org-working-set--circle-continue t))
        (setq org-working-set--cancel-wait-function nil)
        (setq org-working-set--short-help-wanted nil)))
    (define-key kmap (vector ?d)
      (lambda () (interactive)
        (setq this-command last-command)
        (org-working-set--nodes-persist)
        (org-working-set--message (concat (org-working-set--delete-from) " "
                                  (org-working-set--circle-continue)))
        (setq org-working-set--cancel-wait-function nil)))
    (define-key kmap (kbd "<escape>")
      (lambda () (interactive)
        (if org-working-set-clock-into-working-set
            (org-working-set--message "Bailing out of circle, no clock in"))
        (org-working-set--circle-finished-helper t)))
    (define-key kmap (kbd "C-g")
      (lambda () (interactive)
        (if org-working-set--circle-before-marker
            (org-goto-marker-or-bmk org-working-set--circle-before-marker))
        (if org-working-set--circle-win-config
            (set-window-configuration org-working-set--circle-win-config))
        (message "Quit")
        (org-working-set--circle-finished-helper nil)))
    
    (setq org-working-set--cancel-wait-function
          (set-transient-map
           kmap t
           ;; this is run (in any case) on leaving the map
           (lambda () (cancel-timer org-working-set--cancel-timer)
             (message nil)
             ;; Clean up overlay
             (if org-working-set--overlay (delete-overlay org-working-set--overlay))
             (setq org-working-set--overlay nil)
             (if (and org-working-set-clock-into-working-set
                      (not (member (org-id-get) org-working-set--ids-do-not-clock))
                      (not org-working-set--circle-bail-out))
                 (let (keys)
                   ;; save and repeat terminating key, because org-clock-in might read interactively
                   (if (input-pending-p) (setq keys (read-key-sequence nil)))
                   (ignore-errors (org-with-limited-levels (org-clock-in)))
                   (if keys (setq unread-command-events (listify-key-sequence keys)))))
             (if org-working-set--circle-before-marker (move-marker org-working-set--circle-before-marker nil))
             (setq org-working-set--circle-bail-out nil))))

    ;; first move
    (org-working-set--message (org-working-set--circle-continue t))))


(defun org-working-set--circle-finished-helper (bail-out)
  "Common steps on finishing of working set circle.  Argument BAIL-OUT, if t, avoids clocking in."
  (if org-working-set--overlay (delete-overlay org-working-set--overlay))
  (setq org-working-set--overlay nil)
  (setq org-working-set--circle-bail-out bail-out)
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
    (when org-working-set-show-working-set-overlay
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
             (if org-working-set-goto-bottom-in-working-set "bottom of " ""))
     ;; count of nodes
     (if (cdr org-working-set--ids)
         (format " node (out of %d)" (length org-working-set--ids))
       (format " single node"))
     ;; help text
     (if org-working-set--short-help-wanted
         "; type 'c' or space to jump to next node in circle; 'h' for heading, 'b' for bottom of node; type 'd' to delete this node from list; 'q',<return> accepts current position and clocks in, <escape> skips clocking in; <backspace> proceeds in reverse order, 'm' or 'w' switch to working set menu, C-g returns to initial position"
       "; type c,space,h,b,d,q,ret,esc,bs,m,w or ? for short help"))))


(defun org-working-set--menu ()
  "Show menu to let user choose among working-set nodes."

  (setq  org-working-set--short-help-wanted nil)
  (pop-to-buffer org-working-set--menu-buffer-name '((display-buffer-at-bottom)))
  (org-working-set--menu-rebuild t)

  (org-working-set--menu-install-keyboard-shortcuts)
  "Buffer with nodes of working-set")


(defun org-working-set--menu-install-keyboard-shortcuts ()
  "Install keyboard shortcuts for working-set menu.
See `org-working-set--menu-rebuld' for a list of commands."
  (let (keymap)
    (setq keymap (make-sparse-keymap))
    (set-keymap-parent keymap org-mode-map)

    ;; various keys to jump to node
    (mapc (lambda (x) (define-key keymap (kbd x)
                   (lambda () (interactive)
                     (org-working-set--menu-action x))))
          (list "<return>" "<S-return>" "RET" "<tab>" "<S-tab>" "h" "H" "b" "B"))

    (define-key keymap (kbd "p")
      (lambda () (interactive)
        (save-window-excursion
          (save-excursion
            (org-working-set--goto-id (org-working-set--menu-get-id))
            (delete-other-windows)
            (recenter 1)
            (read-char "Peeking into node, any key to return." nil 10)))))

    (define-key keymap (kbd "d")
      (lambda () (interactive)
        (message (org-working-set--delete-from (org-working-set--menu-get-id)))
        (org-working-set--nodes-persist)
        (org-working-set--menu-rebuild)))

    (mapc (lambda (x) (define-key keymap (kbd x)
                   (lambda () (interactive)
                     (let ((id (org-working-set--menu-get-id)))
                       (setq org-working-set--ids-do-not-clock
                             (if (member id org-working-set--ids-do-not-clock)
                                 (delete id org-working-set--ids-do-not-clock)
                               (cons id org-working-set--ids-do-not-clock)))
                       (org-working-set--nodes-persist)
                       (org-working-set--menu-rebuild)))))
          (list "c" "~"))

    (define-key keymap (kbd "u")
      (lambda () (interactive)
        (message (org-working-set--nodes-restore))
        (org-working-set--nodes-persist)
        (org-working-set--menu-rebuild t)))

    (define-key keymap (kbd "q")
      (lambda () (interactive)
        (delete-windows-on org-working-set--menu-buffer-name)
        (kill-buffer org-working-set--menu-buffer-name)))

    (define-key keymap (kbd "r")
      (lambda () (interactive)
        (org-working-set--menu-rebuild)))

    (define-key keymap (kbd "?")
      (lambda () (interactive)
        (setq org-working-set--short-help-wanted (not org-working-set--short-help-wanted))
        (org-working-set--menu-rebuild t)))

    (use-local-map keymap)))


(defun org-working-set--menu-action (key)
  "Perform some actions for working-set menu.
Argument KEY has been pressed to trigger this function."
  (setq key (intern key))
  (let (id)
    (setq id (org-working-set--menu-get-id))
    (cl-case key
      ((<return> <S-return> RET h b)
       (delete-window)
       (org-working-set--goto-id id)
       (recenter 1))
      ((<tab> <S-tab> H B)
       (other-window 1)
       (org-working-set--goto-id id)))
    (if (or (memq key '(b B))
            (and (memq key '(<return> <S-return> RET))
                 org-working-set-goto-bottom-in-working-set)) (org-working-set--bottom-of-node))
    (when (and (not (memq key '(<S-return> <S-tab>)))
               (not (member id org-working-set--ids-do-not-clock)))
      (setq org-working-set--id-last-goto id)
      (if org-working-set-clock-into-working-set (org-with-limited-levels (org-clock-in))))))


(defun org-working-set--menu-get-id ()
  "Extract id from current line in working-set menu."
  (or (get-text-property (point) 'org-working-set-id)
      (error "This line does not point to a node from working-set")))


(defun org-working-set--menu-rebuild (&optional resize)
  "Rebuild content of working-set menu-buffer.
Optional argument RESIZE adjusts window size."
  (let (cursor-here lb)
    (with-current-buffer (get-buffer-create org-working-set--menu-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize (if org-working-set--short-help-wanted
                              (org-working-set--wrap "List of working-set nodes. Pressing <return> on a list element jumps to node in other window and deletes this window, <tab> does the same but keeps this window, <S-return> and <S-tab> do not clock do not clock, `h' and `b' jump to bottom of node unconditionally (with capital letter in other windows), `p' peeks into node from current line, `d' deletes node from working-set immediately, `u' undoes last delete, `q' aborts and deletes this buffer, `r' rebuilds its content, `c' or `~' toggles clocking. Markers on nodes are: `*' for last visited and `~' do not clock.")
                            "Press <return>,<S-return>,<tab>,<S-tab>,h,H,b,B,p,d,u,q,r,c,~,* or ? to toggle short help.")
                          'face 'org-agenda-dimmed-todo-face))
      (insert "\n\n")
      (setq cursor-here (point))
      (if org-working-set--ids
          (mapconcat (lambda (id)
                       (let (head)
                         (save-window-excursion
                           (save-excursion
                             (org-id-goto id)
                             (setq head (concat (substring-no-properties (org-get-heading))
                                                (propertize (concat " / "
                                                                    (org-format-outline-path (reverse (org-get-outline-path)) most-positive-fixnum nil " / "))
                                                            'face 'org-agenda-dimmed-todo-face)))))
                         (let ((prefix1 " ") (prefix2 " "))
                           (if (member id org-working-set--ids-do-not-clock)
                               (setq prefix2 "~"))
                           (when (eq id org-working-set--id-last-goto)
                             (setq prefix1 "*"))
                           (insert (format "%s%s %s" prefix1 prefix2 head)))
                         (setq lb (line-beginning-position))
                         (insert "\n")
                         (put-text-property lb (point) 'org-working-set-id id)))
                     org-working-set--ids
                     "\n")
        (insert "  No nodes in working-set.\n"))
      (goto-char cursor-here)
      (when resize
        (fit-window-to-buffer (get-buffer-window))
        (enlarge-window 1))
      (setq buffer-read-only t))))


(defun org-working-set--goto-id (id)
  "Goto node with given ID and unfold."
  (let (marker)
    (unless (setq marker (org-id-find id 'marker))
      (setq org-working-set--id-last-goto nil)
      (error "Could not find working-set node with id %s" id))
    
    (pop-to-buffer-same-window (marker-buffer marker))
    (goto-char (marker-position marker))
    (org-working-set--unfold-buffer)
    (move-marker marker nil)
    (when org-working-set-goto-bottom-in-working-set
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
      (setq org-working-set--ids-do-not-clock (cl-intersection org-working-set--ids-do-not-clock org-working-set--ids))
      (setq org-working-set--ids (cl-remove-duplicates org-working-set--ids :test (lambda (x y) (string= x y))))
      (setq org-working-set--ids-do-not-clock (cl-remove-duplicates org-working-set--ids-do-not-clock :test (lambda (x y) (string= x y))))
      (org-entry-put (cdr bp) "working-set-nodes" (mapconcat #'identity org-working-set--ids " "))
      (org-entry-put (cdr bp) "working-set-nodes-do-not-clock" (mapconcat #'identity org-working-set--ids-do-not-clock " ")))))


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


(defun org-working-set--wrap (text)
  "Wrap TEXT at fill column."
  (with-temp-buffer
    (insert text)
    (fill-region (point-min) (point-max) nil t)
    (buffer-string)))


(defun org-working-set--unfold-buffer ()
  "Helper function to unfold buffer."
  (org-show-context 'tree)
  (org-reveal '(16))
  (recenter 1))


(defun org-working-set--id-bp ()
  "Return buffer of working-set node."
  (let (marker ret)
    (setq marker (org-id-find org-working-set-id 'marker))
    (unless marker (error "Could not find node %s" org-working-set-id))
    (setq ret (cons (marker-buffer marker)
                    (marker-position marker)))
    (move-marker marker nil)
    ret))


(defun org-working-set--set-id-assistant ()
  "Assist the used in choosing a node where the list of working-set nodes can be stored."
  (let ((assistant-buffer-name "*org working-set assistant*")
        (window-config (current-window-configuration))
        (current-heading (ignore-errors (org-get-heading)))
        use-current-node)

    (pop-to-buffer assistant-buffer-name '((display-buffer-at-bottom)) nil)
    (with-current-buffer assistant-buffer-name
      (erase-buffer)
      (org-mode)
      (mapc (lambda (x) (insert x) (org-fill-paragraph) (insert "\n"))
            (list
             "\nThe required variable `org-working-set-id' has not been set. It should contain the id of a node, where org-working-set will store its runtime information within two special properties. The rest of the node will not be touched."
             "\nThere are three ways to set `org-working-set-id':"
             "- Choose a node and copy the value of its ID-property; use the customize-interface to set `org-working-set-id' to the chosen id."
             "- As above, but edit your .emacs and insert a setq-clause."
             (format "- Use the ID of the node the cursor is currently positioned in (which is '%s')." current-heading)
             "\nIf you choose the first or second way, you should answer 'no' to the question below and go ahead yourself."
             "\nIf you choose the third way, you should answer 'yes'."
             (format "\nHowever, if you are not already within the right node, you may answer 'no' to the question below, navigate to the right node and invoke `%s' again." this-command)))
      (setq mode-line-format nil)
      (setq buffer-read-only t)
      (setq cursor-type nil)
      (fit-window-to-buffer)
      (enlarge-window 1)
      (goto-char (point-min))
      (recenter 0)
      (setq window-size-fixed 'height)

      (unwind-protect
          (setq use-current-node (yes-or-no-p "Do you want to use the id of the current node ? "))
        (kill-buffer-and-window)
        (set-window-configuration window-config)))

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
