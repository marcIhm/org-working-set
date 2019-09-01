;;; org-working-set.el --- Manage a working-set of org-nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2019 Free Software Foundation, Inc.

;; Author: Marc Ihm <1@2484.de>
;; URL: https://github.com/marcIhm/org-working-set
;; Version: 0.0.2
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
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
;;
;; Setup:
;;
;;  - org-working-set can be installed with package.el
;;  - Invoke `M-x org-customize', group 'Org Working-set', and
;;    set `org-working-set-id'.
;;

;;; Change Log:

;;   Version 0.0.2
;;
;;   - Moved functions for working set into its own file
;;   - Show breadcrumbs in working-set-menu
;;
;;  See the package org-index for older news

;;; Code:

(defvar org-ws--ids nil "Ids of working-set nodes (if any).")
(defvar org-ws--ids-do-not-clock nil "Subset of `org-ws--ids', that are not clocked.")
(defvar org-ws--ids-saved nil "Backup for ‘org-ws--ids’.")
(defvar org-ws--id-last-goto nil "Id of last node from working-set, that has been visited.")
(defvar org-ws--circle-before-marker nil "Marker for position before entry into circle.")
(defvar org-ws--circle-win-config nil "Window configuration before entry into circle.")
(defvar org-ws--last-message nil "Last message issued by working-set commands.")
(defvar org-ws--circle-bail-out nil "Set, if bailing out of working-set circle.")
(defvar org-ws--cancel-wait-function nil "Function to call on timeout for working-set commands.")
(defvar org-ws--cancel-timer nil "Timer to cancel waiting for key.")
(defvar org-ws--overlay nil "Overlay to display name of current working-set node.")
(defvar org-ws--short-help-wanted nil "Non-nil, if short help should be displayed in working-set menu.")

(defconst org-ws--menu-buffer-name "*working-set of org-nodes*" "Name of buffer with list of working-set nodes.")

;; Version of this package
(defvar org-working-set-version "0.0.2" "Version of `org-ẃorking-set', format is major.minor.bugfix, where \"major\" are incompatible changes and \"minor\" are new features.")

;; customizable options
(defgroup org-working-set nil
  "Options concerning the optional working-set of org-nodes."
  :tag "Org Working-set"
  :group 'org)

(defcustom org-working-set-id nil
  "Id of the Org-mode node, which contains the index table. This can be set to the id of any node you like; only its property drawer will be used. "
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

This is version 0.0.1 of org-working-set.el.

The subcommands allow to:
- Modify the list of nodes (e.g. add new nodes)
- Circle quickly through the nodes
- Show a menu buffer with all nodes currently in the working set

Optional argument SILENT does not issue final message."
  (interactive)

  (unless org-working-set-id
    (error "Variable org-working-set-id is not set."))
  (unless org-ws--ids
    (let ((bp (org-ws--id-bp)))
      (with-current-buffer (car bp)
        (save-excursion
          (goto-char (cdr bp))
          (setq org-ws--ids (split-string (or (org-entry-get nil "working-set-nodes") "")))
          (setq org-ws--ids-do-not-clock (split-string (or (org-entry-get nil "working-set-nodes-do-not-clock") "")))))))
  
  (let ((char-choices (list ?s ?S ?a ?A ?d ?u ?w ?m ?c ?g ? ??))
        id text more-text char prompt ids-up-to-top)

    (setq prompt (format "Please specify action on working-set of %d nodes (s,S,a,A,d,u,m,w,c,space,g or ? for short help) - " (length org-ws--ids)))
    (while (or (not (memq char char-choices))
               (= char ??))
      (setq char (read-char-choice prompt char-choices))
      (setq prompt (format "Actions on working-set of %d nodes:  s)et working-set to this node alone,  S)et but do not clock,  a)ppend this node to set,  A)ppend but do not clock,  d)elete this node from list,  u)ndo last modification of working set,  w),m)enu to edit working set (same as 'w'),  c),space) enter working set circle,  g)o to bottom position in current node.  Please choose - " (length org-ws--ids))))

    (when (and (memq char (list ?s ?S ?a ?A ?d))
               (not (string= major-mode "org-mode")))
      (error "Current buffer is not in org-mode"))

    (setq text
          (cond

           ((or (eq char ?s)
                (eq char ?S))
            (setq id (org-id-get-create))
            (setq org-ws--ids-saved org-ws--ids)
            (setq org-ws--ids (list id))
            (if (eq char ?S)
                (setq org-ws--ids-do-not-clock (list id))
              (setq org-ws--id-last-goto id)
              (if org-working-set-clock-into-working-set (org-with-limited-levels (org-clock-in))))
            "working-set has been set to current node (1 node)")

           ((or (eq char ?a)
                (eq char ?A))
            (setq id (org-id-get-create))
            (unless (member id org-ws--ids)
              ;; remove any children, that are already in working-set
              (setq org-ws--ids
                    (delete nil (mapcar (lambda (x)
                                          (if (member id (org-with-point-at (org-id-find x t)
                                                           (org-ws--ids-up-to-top)))
                                              (progn
                                                (setq more-text ", removing its children")
                                                nil)
                                            x))
                                        org-ws--ids)))
              (setq org-ws--ids-saved org-ws--ids)
              ;; remove parent, if already in working-set
              (setq ids-up-to-top (org-ws--ids-up-to-top))
              (when (seq-intersection ids-up-to-top org-ws--ids)
                (setq org-ws--ids (seq-difference org-ws--ids ids-up-to-top))
                (setq more-text (concat more-text ", replacing its parent")))
              (setq org-ws--ids (cons id org-ws--ids)))
            (if (eq char ?A)
                (setq org-ws--ids-do-not-clock (cons id org-ws--ids-do-not-clock))
              (setq org-ws--id-last-goto id)
              (if org-working-set-clock-into-working-set (org-with-limited-levels (org-clock-in))))
            "current node has been appended to working-set%s (%d node%s)")

           ((eq char ?d)
            (org-ws--delete-from))

           ((memq char '(?m ?w))
            (org-ws--menu))

           ((memq char '(?c ? ))
            (org-ws--circle-start))

           ((eq char ?g)
            (org-ws--bottom-of-node)
            "at bottom of node")

           ((eq char ?u)
            (org-ws--nodes-restore))))

    (org-ws--nodes-persist)
    
    (setq text (format text (or more-text "") (length org-ws--ids) (if (cdr org-ws--ids) "s" "")))
    (unless silent (message (concat (upcase (substring text 0 1)) (substring text 1))))
    text))


(defun org-ws--circle-start ()
  "Go through working-set, one node after the other."
  (unless org-ws--ids (error "No nodes in working-set; need to add some first"))

  (setq org-ws--short-help-wanted nil)
  (setq org-ws--circle-before-marker (point-marker))
  (setq org-ws--circle-win-config (current-window-configuration))

  (let ((kmap (make-sparse-keymap)))
    (mapc (lambda (x)
            (define-key kmap (vector x)
              (lambda () (interactive)
                (setq this-command last-command)
                (org-ws--message (org-ws--circle-continue)))))
          (list ?c ? ))
    (mapc (lambda (x)
            (define-key kmap (kbd x)
              (lambda () (interactive)
                (org-ws--message "Circle done")
                (org-ws--circle-finished-helper nil))))
          (list "RET" "<return>" "q"))
    (mapc (lambda (x)
            (define-key kmap (kbd x)
              (lambda () (interactive)
                (setq this-command last-command)
                (org-ws--message (org-ws--circle-continue nil t)))))
          (list "DEL" "<backspace>"))
    (mapc (lambda (x)
            (define-key kmap (kbd x)
              (lambda () (interactive)
                (org-ws--message "Switching to menu")
                (org-ws--circle-finished-helper t)
                (run-with-timer 0 nil 'org-ws--menu))))
          (list "w" "m"))
    (define-key kmap (vector ?h)
      (lambda () (interactive)
        (org-ws--head-of-node)
        (org-ws--message "On heading of node from working-set")))
    (define-key kmap (vector ?b)
      (lambda () (interactive)
        (org-ws--bottom-of-node)
        (org-ws--message "At bottom of node from working-set")))
    (define-key kmap (vector ??)
      (lambda () (interactive)
        (setq org-ws--short-help-wanted t)
        (message (org-ws--circle-continue t))
        (setq org-ws--cancel-wait-function nil)
        (setq org-ws--short-help-wanted nil)))
    (define-key kmap (vector ?d)
      (lambda () (interactive)
        (setq this-command last-command)
        (org-ws--nodes-persist)
        (org-ws--message (concat (org-ws--delete-from) " "
                                  (org-ws--circle-continue)))
        (setq org-ws--cancel-wait-function nil)))
    (define-key kmap (kbd "<escape>")
      (lambda () (interactive)
        (if org-working-set-clock-into-working-set
            (org-ws--message "Bailing out of circle, no clock in"))
        (org-ws--circle-finished-helper t)))
    (define-key kmap (kbd "C-g")
      (lambda () (interactive)
        (if org-ws--circle-before-marker
            (org-goto-marker-or-bmk org-ws--circle-before-marker))
        (if org-ws--circle-win-config
            (set-window-configuration org-ws--circle-win-config))
        (message "Quit")
        (org-ws--circle-finished-helper)))
    
    (setq org-ws--cancel-wait-function
          (set-transient-map
           kmap t
           ;; this is run (in any case) on leaving the map
           (lambda () (cancel-timer org-ws--cancel-timer)
             (message nil)
             ;; Clean up overlay
             (if org-ws--overlay (delete-overlay org-ws--overlay))
             (setq org-ws--overlay nil)
             (if (and org-working-set-clock-into-working-set
                      (not (member (org-id-get) org-ws--ids-do-not-clock))
                      (not org-ws--circle-bail-out))
                 (let (keys)
                   ;; save and repeat terminating key, because org-clock-in might read interactively
                   (if (input-pending-p) (setq keys (read-key-sequence nil)))
                   (ignore-errors (org-with-limited-levels (org-clock-in)))
                   (if keys (setq unread-command-events (listify-key-sequence keys)))))
             (if org-ws--circle-before-marker (move-marker org-ws--circle-before-marker nil))
             (setq org-ws--circle-bail-out nil))))

    ;; first move
    (org-ws--message (org-ws--circle-continue t))))


(defun org-ws--circle-finished-helper (bail-out)
  "Common steps on finishing of working set circle. Argument bail-out, if t, avoids clocking in."
  (if org-ws--overlay (delete-overlay org-ws--overlay))
  (setq org-ws--overlay nil)
  (setq org-ws--circle-bail-out bail-out)
  (setq org-ws--cancel-wait-function nil))


(defun org-ws--circle-continue (&optional stay back)
  "Continue with working set circle after start.
Optional argument STAY prevents changing location."
  (let (last-id following-id previous-id target-id parent-ids head)

    ;; compute target
    (setq last-id (or org-ws--id-last-goto
                      (car (last org-ws--ids))))
    (setq following-id (car (or (cdr-safe (member last-id
                                                  (append org-ws--ids org-ws--ids)))
                                org-ws--ids)))
    (if back
        (setq previous-id (car (or (cdr-safe (member last-id
                                                     (reverse (append org-ws--ids org-ws--ids))))
                                   org-ws--ids))))
    (setq target-id (if stay last-id (if back previous-id following-id)))
    (setq parent-ids (org-ws--ids-up-to-top)) ; remember this before changing location
    
    ;; bail out on inactivity
    (if org-ws--cancel-timer (cancel-timer org-ws--cancel-timer))
    (setq org-ws--cancel-timer
          (run-at-time 30 nil
                       (lambda () (if org-ws--cancel-wait-function
                                 (funcall org-ws--cancel-wait-function)))))

    (org-ws--goto-id target-id)
    (setq org-ws--id-last-goto target-id)

    ;; tooltip-overlay to show current heading
    (setq head (org-with-limited-levels (org-get-heading t t t t)))
    (when org-working-set-show-working-set-overlay
      (if org-ws--overlay (delete-overlay org-ws--overlay))
      (setq org-ws--overlay (make-overlay (point-at-eol) (point-at-eol)))
      (overlay-put org-ws--overlay
                   'after-string
                   (propertize
                    (format " %s (%d of %d) "
                            head
                            (1+ (- (length org-ws--ids)
                                   (length (member target-id org-ws--ids))))
                            (length org-ws--ids))
                    'face 'match))
      (overlay-put org-ws--overlay 'priority most-positive-fixnum))

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
     (if (cdr org-ws--ids)
         (format " node (out of %d)" (length org-ws--ids))
       (format " single node"))
     ;; help text
     (if org-ws--short-help-wanted
         "; type 'c' or space to jump to next node in circle; 'h' for heading, 'b' for bottom of node; type 'd' to delete this node from list; 'q',<return> accepts current position and clocks in, <escape> skips clocking in; <backspace> proceeds in reverse order, 'm' or 'w' switch to working set menu, C-g returns to initial position"
       "; type c,space,h,b,d,q,ret,esc,bs,m,w or ? for short help"))))


(defun org-ws--menu ()
  "Show menu to let user choose among working-set nodes."

  (setq  org-ws--short-help-wanted nil)
  (pop-to-buffer org-ws--menu-buffer-name '((display-buffer-at-bottom)))
  (org-ws--menu-rebuild t)

  (org-ws--menu-install-keyboard-shortcuts)
  "Buffer with nodes of working-set")


(defun org-ws--menu-install-keyboard-shortcuts ()
  "Install keyboard shortcuts for working-set menu.
See `org-ws--menu-rebuld' for a list of commands."
  (let (keymap)
    (setq keymap (make-sparse-keymap))
    (set-keymap-parent keymap org-mode-map)

    ;; various keys to jump to node
    (mapc (lambda (x) (define-key keymap (kbd x)
                   (lambda () (interactive)
                     (org-ws--menu-action x))))
          (list "<return>" "<S-return>" "RET" "<tab>" "<S-tab>" "h" "H" "b" "B"))

    (define-key keymap (kbd "p")
      (lambda () (interactive)
        (save-window-excursion
          (save-excursion
            (org-ws--goto-id (org-ws--menu-get-id))
            (delete-other-windows)
            (recenter 1)
            (read-char "Peeking into node, any key to return." nil 10)))))

    (define-key keymap (kbd "d")
      (lambda () (interactive)
        (message (org-ws--delete-from (org-ws--menu-get-id)))
        (org-ws--nodes-persist)
        (org-ws--menu-rebuild)))

    (mapc (lambda (x) (define-key keymap (kbd x)
                   (lambda () (interactive)
                     (let ((id (org-ws--menu-get-id)))
                       (setq org-ws--ids-do-not-clock
                             (if (member id org-ws--ids-do-not-clock)
                                 (delete id org-ws--ids-do-not-clock)
                               (cons id org-ws--ids-do-not-clock)))
                       (org-ws--nodes-persist)
                       (org-ws--menu-rebuild)))))
          (list "c" "~"))

    (define-key keymap (kbd "u")
      (lambda () (interactive)
        (message (org-ws--nodes-restore))
        (org-ws--nodes-persist)
        (org-ws--menu-rebuild t)))

    (define-key keymap (kbd "q")
      (lambda () (interactive)
        (delete-windows-on org-ws--menu-buffer-name)
        (kill-buffer org-ws--menu-buffer-name)))

    (define-key keymap (kbd "r")
      (lambda () (interactive)
        (org-ws--menu-rebuild)))

    (define-key keymap (kbd "?")
      (lambda () (interactive)
        (setq org-ws--short-help-wanted (not org-ws--short-help-wanted))
        (org-ws--menu-rebuild t)))

    (use-local-map keymap)))


(defun org-ws--menu-action (key)
  "Perform some actions for working-set menu.
Argument KEY has been pressed to trigger this function."
  (setq key (intern key))
  (let (id)
    (setq id (org-ws--menu-get-id))
    (cl-case key
      ((<return> <S-return> RET h b)
       (delete-window)
       (org-ws--goto-id id)
       (recenter 1))
      ((<tab> <S-tab> H B)
       (other-window 1)
       (org-ws--goto-id id)))
    (if (or (memq key '(b B))
            (and (memq key '(<return> <S-return> RET))
                 org-working-set-goto-bottom-in-working-set)) (org-ws--bottom-of-node))
    (when (and (not (memq key '(<S-return> <S-tab>)))
               (not (member id org-ws--ids-do-not-clock)))
      (setq org-ws--id-last-goto id)
      (if org-working-set-clock-into-working-set (org-with-limited-levels (org-clock-in))))))


(defun org-ws--menu-get-id ()
  "Extract id from current line in working-set menu."
  (or (get-text-property (point) 'org-working-set-id)
      (error "This line does not point to a node from working-set")))


(defun org-ws--menu-rebuild (&optional resize)
  "Rebuild content of working-set menu-buffer.
Optional argument RESIZE adjusts window size."
  (let (cursor-here lb)
    (with-current-buffer (get-buffer-create org-ws--menu-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize (if org-ws--short-help-wanted
                              (org-ws--wrap "List of working-set nodes. Pressing <return> on a list element jumps to node in other window and deletes this window, <tab> does the same but keeps this window, <S-return> and <S-tab> do not clock do not clock, `h' and `b' jump to bottom of node unconditionally (with capital letter in other windows), `p' peeks into node from current line, `d' deletes node from working-set immediately, `u' undoes last delete, `q' aborts and deletes this buffer, `r' rebuilds its content, `c' or `~' toggles clocking. Markers on nodes are: `*' for last visited and `~' do not clock.")
                            "Press <return>,<S-return>,<tab>,<S-tab>,h,H,b,B,p,d,u,q,r,c,~,* or ? to toggle short help.")
                          'face 'org-agenda-dimmed-todo-face))
      (insert "\n\n")
      (setq cursor-here (point))
      (if org-ws--ids
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
                           (if (member id org-ws--ids-do-not-clock)
                               (setq prefix2 "~"))
                           (when (eq id org-ws--id-last-goto)
                             (setq prefix1 "*"))
                           (insert (format "%s%s %s" prefix1 prefix2 head)))
                         (setq lb (line-beginning-position))
                         (insert "\n")
                         (put-text-property lb (point) 'org-working-set-id id)))
                     org-ws--ids
                     "\n")
        (insert "  No nodes in working-set.\n"))
      (goto-char cursor-here)
      (when resize
        (fit-window-to-buffer (get-buffer-window))
        (enlarge-window 1))
      (setq buffer-read-only t))))


(defun org-ws--goto-id (id)
  "Goto node with given ID and unfold."
  (let (marker)
    (unless (setq marker (org-id-find id 'marker))
      (setq org-ws--id-last-goto nil)
      (error "Could not find working-set node with id %s" id))
    
    (pop-to-buffer-same-window (marker-buffer marker))
    (goto-char (marker-position marker))
    (org-ws--unfold-buffer)
    (move-marker marker nil)
    (when org-working-set-goto-bottom-in-working-set
      (org-ws--bottom-of-node))))


(defun org-ws--message (message)
  "Issue given MESSAGE and append string '(again)' if appropriate."
  (let ((again (if (string= message org-ws--last-message) " (again)" "")))
    (setq org-ws--last-message message)
    (message (concat message again "."))))


(defun org-ws--bottom-of-node ()
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


(defun org-ws--head-of-node ()
  "Goto head of current node."
  (org-with-limited-levels (org-back-to-heading))
  (recenter 2))


(defun org-ws--nodes-restore (&optional upcase)
  "Restore previously saved working-set.
Optional argument UPCASE modifies the returned message."
  (let (txt)
    (if org-ws--ids-saved
        (progn
          (setq txt (format "Discarded current working set of and restored previous set; now %d node%s in working-set" (length org-ws--ids-saved) (if (cdr org-ws--ids-saved) "s" "")))
          (setq org-ws--ids org-ws--ids-saved))
      (setq txt "No saved working-set nodes to restore, nothing to do"))
    (if upcase (concat (upcase (substring txt 0 1))
                       (substring txt 1)
                       ".")
      txt)))


(defun org-ws--nodes-persist ()
  "Write working-set to property."
  (let ((bp (org-ws--id-bp)))
    (with-current-buffer (car bp)
      (setq org-ws--ids-do-not-clock (cl-intersection org-ws--ids-do-not-clock org-ws--ids))
      (setq org-ws--ids (cl-remove-duplicates org-ws--ids :test (lambda (x y) (string= x y))))
      (setq org-ws--ids-do-not-clock (cl-remove-duplicates org-ws--ids-do-not-clock :test (lambda (x y) (string= x y))))
      (org-entry-put (cdr bp) "working-set-nodes" (mapconcat 'identity org-ws--ids " "))
      (org-entry-put (cdr bp) "working-set-nodes-do-not-clock" (mapconcat 'identity org-ws--ids-do-not-clock " ")))))


(defun org-ws--delete-from (&optional id)
  "Delete current node from working-set.
Optional argument ID gives the node to delete."
  (setq id (or id (org-id-get)))
  (format
   (if (and id (member id org-ws--ids))
       (progn
         (if (string= id org-ws--id-last-goto) (setq org-ws--id-last-goto nil))
         (setq org-ws--ids-saved org-ws--ids)
         (setq org-ws--ids (delete id org-ws--ids))
         "Current node has been removed from working-set (%d node%s)")
     "Current node has not been in working-set (%d node%s)")
   (length org-ws--ids) (if org-ws--ids "s" "")))


(defun org-ws--ids-up-to-top ()
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


(defun org-ws--wrap (text)
  "Wrap TEXT at fill column."
  (with-temp-buffer
    (insert text)
    (fill-region (point-min) (point-max) nil t)
    (buffer-string)))


(defun org-ws--unfold-buffer ()
  "Helper function to unfold buffer."
  (org-show-context 'tree)
  (org-reveal '(16))
  (recenter 1))


(defun org-ws--id-bp ()
  "Return buffer of working-set node."
  (let (marker ret)
    (setq marker (org-id-find org-working-set-id 'marker))
    (unless marker (error "Could not find node %s" org-working-set-id))
    (setq ret (cons (marker-buffer marker)
                    (marker-position marker)))
    (move-marker marker nil)
    ret))


(defun org-ws--set-id-assistant ()
  "Assist the used in choosing a node where the list of working-set nodes can be stored."
  (let ((org-ws--help-buffer-name "*org working-set help*")))
  (with-current-buffer-window
   org-ws--help-buffer-name nil nil
   (erase-buffer)
   (org-mode)
   (insert "The required variable org-working-set-id has not been set. It should contain the id of a node, where org-working-set stores its runtime information within two special propertie; the rest of the node will not be touched.")
   (princ (or prompt "Short help; shortcuts in []; capital letter acts like C-u.\n"))
   (princ (or choices (oidx--get-short-help-text))))
  (with-current-buffer oidx--short-help-buffer-name
    (let ((inhibit-read-only t))
      (setq mode-line-format nil)
      (setq cursor-type nil)
      (fit-window-to-buffer (get-buffer-window))
      (setq window-size-fixed 'height)
      (goto-char (point-min))
      (end-of-line)))

  (let (buffer
        title
        firstref
        id)
    
    (setq title (read-from-minibuffer "Please enter the title of the index node (leave empty for default 'index'): "))
    (if (string= title "") (setq title "index"))
    
    (while (progn
             (setq firstref (read-from-minibuffer "Please enter your first reference-number. This is an integer number preceeded by some and optionally followed by some non-numeric chars; e.g. 'R1', '-1-' or '#1#' (and your initial number does not need to be '1'). The format of your reference-numbers only needs to make sense for yourself, so that you can spot it easily in your texts or write it on a piece of paper; it should however not already appear frequently within your existing notes, to avoid too many false hits when searching.\n\nPlease choose (leave empty for default 'R1'): "))
             (if (string= firstref "") (setq firstref "R1"))
             (let (desc)
               (when (string-match "[[:blank:]]" firstref)
                 (setq desc "Contains whitespace"))
               (when (string-match "[[:cntrl:]]" firstref)
                 (setq desc "Contains control characters"))
               (unless (string-match "^[^0-9]+[0-9]+[^0-9]*$" firstref)
                 ;; firstref not okay, report details
                 (setq desc
                       (cond ((string= firstref "") "is empty")
                             ((not (string-match "^[^0-9]+" firstref)) "starts with a digit")
                             ((not (string-match "^[^0-9]+[0-9]+" firstref)) "does not contain a number")
                             ((not (string-match "^[^0-9]+[0-9]+[^0-9]*$" firstref)) "contains more than one sequence of digits"))))
               (if desc
                   (progn
                     (read-from-minibuffer (format "Your input '%s' does not meet the requirements because it %s.\nPlease hit RET and try again: " firstref desc))
                     t)
                 nil))))

    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "\n* %s %s\n" firstref title))
      (org-entry-put (point) "max-ref" firstref)
      (unless oidx--recording-screencast
	(if temporary
            (insert "
  Below you find your temporary index table, which WILL NOT LAST LONGER
  THAN YOUR CURRENT EMACS SESSION; please use it only for evaluation.
")
          (insert "
  Below you find your initial index table, which will grow over time.
"))
	(insert "  You may start using it by adding some lines. Just
  move to another heading within org, invoke `org-index' and
  choose the command 'add'.  After adding a few nodes, try the
  command 'occur' to search among them.

  To gain further insight you may invoke the subcommand 'help', or
  (with the same content) read the help of `org-index'.

  Invoke `org-customize' to tweak the behaviour of org-index,
  see the group org-index. It might be useful to set the global
  key `org-index-key'.

  This node needs not be a top level node; its name is completely
  at your choice; it is found through its ID only.

  You may change the order of columns in this table; if you do
  so, please consider adjusting `org-index-occur-columns'.
  Additional custom columns can be added, if they start with
  a dot.
")
	(unless temporary
          (insert "
  Remark: These lines of explanation can be removed at any time.
")))

      (setq id (org-id-get-create))
      (insert (format "

  | ref | category | keywords | tags | count | level | last-accessed | created | id  | yank |
  |     |          |          |      |       |       |               |         | <4> | <4>  |
  |-----+----------+----------+------+-------+-------+---------------+---------+-----+------|
  | %s  |          | %s       |      |       |       |               | %s      | %s  |      |

"
                      firstref
                      title
                      (with-temp-buffer (org-insert-time-stamp nil nil t))
                      id))

      ;; make sure, that node can be found
      (org-id-add-location id (buffer-file-name))
      (setq buffer-save-without-query t)
      (basic-save-buffer)

      (while (not (org-match-line org-table-line-regexp)) (forward-line -1))
      (unless buffer-read-only (org-table-align))
      (while (not (org-at-heading-p)) (forward-line -1))

      ;; read back some info about new index
      (let ((org-index-id id))
	(oidx--verify-id))

      ;; remember at least for this session
      (setq org-index-id id)

      ;; present results to user
      (if temporary
          (progn
            ;; Present existing and temporary index together
            (when compare
              (pop-to-buffer-same-window oidx--buffer)
              (goto-char oidx--point)
              (oidx--unfold-buffer)
              (delete-other-windows)
              (select-window (split-window-vertically)))
            ;; show new index
            (pop-to-buffer-same-window buffer)
            (org-id-goto id)
            (oidx--unfold-buffer)
            (if compare
                (progn
                  (message "Please compare your existing index (upper window) and a temporary new one (lower window) to fix your index")
                  (throw 'new-index nil))
              (message "This is your new temporary index, use command add to populate, occur to search.")))
        (progn
          ;; Show the new index
          (pop-to-buffer-same-window buffer)
          (delete-other-windows)
          (org-id-goto id)
          (oidx--unfold-buffer)
          (if (y-or-n-p "This is your new index table.  It is already set for this Emacs session, so you may try it out.  Do you want to save it's id to make it available in future Emacs sessions too ? ")
              (progn
                (customize-save-variable 'org-index-id id)
                (message "Saved org-index-id '%s' to %s." id (or custom-file user-init-file)))
            (let (sq)
              (setq sq (format "(setq org-index-id \"%s\")" id))
              (kill-new sq)
              (message "Did not make the id of this new index permanent; you may want to put\n\n   %s\n\ninto your own initialization; it is copied already, just yank it." sq)))

          (when (not org-index-key)
            (if (y-or-n-p "The central function `org-index' can be bound to a global key.  Do you want to make such a binding for now ? ")
	        (let ((prompt (concat "Please type your desired key sequence. For example, with the user-prefix key C-c, these keys are available: " (mapconcat 'char-to-string (remove nil (mapcar (lambda (c) (if (key-binding (kbd (format "C-c %c" c))) nil c)) (number-sequence ?a ?z))) ",") ". But of course, you may choose any free key-sequence you like (C-g to cancel): "))
		      (preprompt "")
		      key)
	          (while (progn
		           (setq key (read-key-sequence (concat preprompt prompt)))
		           (setq preprompt (format "Key '%s' is already taken; please choose another one. " (kbd key)))
		           (and (key-binding key)
			        (not (string= (kbd key) (kbd "^g"))))))
	          (if (string= (kbd key) (kbd "^g"))
                      (message "Aborted")
		    (global-set-key key 'org-index)
		    (let ((saved ""))
		      (when (y-or-n-p "Do you want to save this for future Emacs sessions ? ")
		        (customize-save-variable 'org-index-key key)
		        (setq saved "and saved "))
		      (message "Set %sorg-index-key '%s' to %s." saved (kbd key) (or custom-file user-init-file)))))
	      (message "Did not set org-index-key; however this can be done any time with `org-customize'.")))
          (throw 'new-index nil))))))


(provide 'org-working-set)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; org-working-set.el ends here
