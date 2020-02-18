;;; setup_org.el --- Setting up org mode
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; This file is created based on the author's coding habit.
;; This may not be suitable for some people.

;;; Commentary:
;;
;; Setting up org mode
;; -----------------------------------------------------------

;;; Code:
(require 'setup_misc)
(require 'setup_hydra)

;; Setup `Org'
(use-package org
  :ensure t
  :delight "Θ "
  :bind ("C-c i" . org-insert-structure-template)
  :preface
  (defun penguin/org-compare-times (clocked estimated)
    "Gets the ratio between the timed time and the estimated time."
    (if (and (> (length clocked) 0) estimated)
        (format "%.2f"
                (/ (* 1.0 (org-hh:mm-string-to-minutes clocked))
                   (org-hh:mm-string-to-minutes estimated)))
      "")
  )
  (defun penguin/org-archive-done-tasks ()
    "Archives finished or cancelled tasks."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "TODO=\"DONE\"|TODO=\"CANCELLED\"" (if (org-before-first-heading-p) 'file 'tree))
  )
  (defun penguin/org-jump ()
    "Jumps to a specific task."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'org-refile))
  )
  (defun penguin/org-use-speed-commands-for-headings-and-lists ()
    "Activates speed commands on list items too."
    (or (and (looking-at org-outline-regexp) (looking-back "^\**"))
        (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*"))))
  )
  (defmacro ignore-args (fnc)
    "Returns function that ignores its arguments and invokes FNC."
    `(lambda (&rest _rest)
       (funcall ,fnc))
  )
  :hook ((after-save . penguin/config-tangle)
         (auto-save . org-save-all-org-buffers)
         (org-mode . org-indent-mode))
  :custom
  ;; (org-archive-location "~/.emacs.d/.private/org/.archives/%s::")
  (org-blank-before-new-entry '((heading . t)
                                (plain-list-item . t)))
  (org-cycle-include-plain-lists 'integrate)
  (org-expiry-inactive-timestamps t)
  (org-export-backends '(ascii beamer html icalendar latex man md org texinfo))
  (org-log-done 'time)
  (org-log-into-drawer "LOGBOOK")
  (org-modules '(org-crypt
                 org-habit
                 org-info
                 org-irc
                 org-mouse
                 org-protocol
                 org-tempo))
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-cache nil)
  (org-refile-use-outline-path nil)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-startup-folded nil)
  (org-startup-with-inline-images t)
  (org-tag-alist '(("@coding" . ?c)
                   ("@computer" . ?l)
                   ("@home" . ?h)
                   ("@phone" . ?p)
                   ("@reading" . ?r)
                   ("@work" . ?b)
                   ("@writing" . ?w)
                   ("crypt" . ?C)
                   ("fuzzy" . ?0)
                  )
  )
  (org-tags-exclude-from-inheritance '("crypt" "project"))
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                       (sequence "NOTE(N)" "MEETING(M)")
                       (sequence "WAITING(w@)" "DELEGATED(e@)" "LATER(l)" "|" "CANCELLED(c)"))
  )

  (org-use-effective-time t)
  (org-use-speed-commands 'penguin/org-use-speed-commands-for-headings-and-lists)
  (org-yank-adjusted-subtrees t)
  :config
  (add-to-list 'org-global-properties '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))
  (add-to-list 'org-speed-commands-user '("!" penguin/org-clock-in-and-track))
  (add-to-list 'org-speed-commands-user '("$" call-interactively 'org-archive-subtree))
  (add-to-list 'org-speed-commands-user '("d" penguin/org-move-line-to-destination))
  (add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
  (add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
  (add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
  (add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
  (add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
  (advice-add 'org-deadline :after (ignore-args #'org-save-all-org-buffers))
  (advice-add 'org-schedule :after (ignore-args #'org-save-all-org-buffers))
  (advice-add 'org-store-log-note :after (ignore-args #'org-save-all-org-buffers))
  (advice-add 'org-todo :after (ignore-args #'org-save-all-org-buffers))
  (org-clock-persistence-insinuate)
  (org-load-modules-maybe t)
)
(use-package toc-org
  :after org
  :hook (org-mode . toc-org-enable)
)
(use-package org-indent :ensure nil :after org :delight)
(use-package org-faces
  :ensure nil
  :after org
  :custom
  (org-todo-keyword-faces '(("WAITING" :foreground "#fabd2f" :weight bold)
                            ("DELEGATED" :foreground "#fabd2f" :weight bold)
                            ("NOTE" :foreground "#83a598" :weight bold)
                            ("MEETING" :foreground "#83a598" :weight bold)
                            ("LATER" :foreground "#83a598" :weight bold)
                            ("NEXT" :foreground "#b8bb26" :weight bold))
  )
)

;; Setup `Org-agenda'
(use-package org-agenda
  :ensure nil
  :after org
  :bind (:map org-agenda-mode-map
              ("X" . penguin/org-agenda-mark-done-and-add-followup)
              ("x" . penguin/org-agenda-done))
  :preface
  (defun penguin/org-agenda-done (&optional arg)
    "Mark current TODO as done.
  This changes the line at point, all other lines in the agenda referring to
  the same tree node, and the headline of the tree node in the Org-mode file."
    (interactive "P")
    (org-agenda-todo "DONE")
  )
  (defun penguin/org-agenda-mark-done-and-add-followup ()
    "Mark the current TODO as done and add another task after it.
   Creates it at the same level as the previous task, so it's better to use
   this with to-do items than with projects or headings."
    (interactive)
    (org-agenda-todo "DONE")
    (org-agenda-switch-to)
    (org-capture 0 "t")
  )
  :custom
  (org-agenda-dim-blocked-tasks t)
  (org-directory "~/.emacs.d/.private/org/")
  ;; (org-default-notes-file "~/.personal/agenda/organizer.org")
  (org-agenda-files (list org-directory))
  (org-agenda-inhibit-startup t)
  (org-agenda-show-log t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-span 2)
  (org-agenda-start-on-weekday 6)
  (org-agenda-sticky nil)
  (org-agenda-tags-column -100)
  (org-agenda-time-grid '((daily today require-timed)))
  (org-agenda-use-tag-inheritance t)
  (org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS")
  (org-enforce-todo-dependencies t)
  (org-habit-completed-glyph ?✓)
  (org-habit-graph-column 80)
  (org-habit-show-habits-only-for-today nil)
  (org-habit-today-glyph ?‖)
  (org-track-ordered-property-with-tag t)
)
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○"))
  (org-hide-emphasis-markers nil)
  (org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+")))
  (org-ellipsis "▼")
  (org-emphasis-alist '(("*" (bold :foreground "Orange" ))
                        ("/" italic)
                        ("_" underline)
                        ("=" (:foreground "maroon"))
                        ("~" (:foreground "deep sky blue"))
                        ("+" (:strike-through t)))
  )
)
(use-package org-super-agenda)
(setq penguin-org-deadline-prefix "%2i%-12(penguin-agenda-prefix)")
(setq org-agenda-custom-commands '(("n" "Agenda"
                                    ((agenda "")
                                     (todo ""
                                           ((org-agenda-overriding-header (concat (all-the-icons-faicon "chain-broken" :v-adjust 0.01) " Stuck Projects"))
                                            (org-agenda-skip-function #'penguin/should-skip)
                                            (org-agenda-prefix-format penguin-org-deadline-prefix)
                                            (org-agenda-sorting-strategy nil)))
                                     (todo "NEXT"
                                           ((org-agenda-overriding-header (concat (all-the-icons-faicon "bolt" :v-adjust 0.01) " Next Tasks"))
                                            (org-agenda-sorting-strategy
                                             '(priority-down category-up))))
                                     (todo "TODO"
                                           ((org-agenda-files '("~/.emacs.d/.private/org/todo.org" "~/.emacs.d/.private/org/notes.org"))
                                            (org-agenda-sorting-strategy
                                             '(priority-down category-up))
                                            (org-agenda-overriding-header (concat (all-the-icons-faicon "check-square-o" :v-adjust 0.01) " Tasks"))))
                                     (todo "WAITING|DELEGATED"
                                           ((org-agenda-overriding-header (concat (all-the-icons-faicon "hourglass" :v-adjust 0.01) " Waiting/Delegated"))
                                            (org-agenda-sorting-strategy '(priority-down category-up))))
                                     (todo "LATER"
                                           ((org-agenda-sorting-strategy
                                             '(priority-down category-up))
                                            (org-agenda-overriding-header (concat (all-the-icons-faicon "thumb-tack" :v-adjust 0.01) " Later"))))
                                     (todo "NOTE"
                                           ((org-agenda-overriding-header (concat (all-the-icons-faicon "sticky-note" :v-adjust 0.01) " Notes"))
                                            (org-agenda-max-entries 10)
                                            (org-agenda-sorting-strategy
                                             '(tsia-down)))))

                                    nil
                                   )
                                  )
)


;; Setup `Org-capture'
(use-package org-capture
  :ensure nil
  :after org
  :custom
  (+org-capture-todo-file "~/.emacs.d/.private/org/todo.org")
  (+org-capture-notes-file "~/.emacs.d/.private/org/notes.org")
  (org-capture-templates '(("t" "Todo" entry
                            (file+headline +org-capture-todo-file "Inbox")
                            "* TODO %?" :prepend t :kill-buffer t)
                           ("n" "Next" entry
                            (file+headline +org-capture-todo-file "Inbox")
                            "* NEXT %?" :prepend t :kill-buffer t)
                           ("w" "Waiting" entry
                            (file+headline +org-capture-todo-file "Inbox")
                            "* WAITING %?" :prepend t :kill-buffer t)
                           ("o" "Email Note" entry
                            (file+headline +org-capture-notes-file "Inbox")
                            "* NOTE %u %^{Content?} :email:\n%a" :prepend t :kill-buffer t)
                           ("e" "Email" entry
                            (file+headline +org-capture-todo-file "Inbox")
                            "* %^{Type?|TODO|NEXT|WAITING|DELEGATED} %^{Content?} :email:\nSCHEDULED: %t\n%a" :prepend t :kill-buffer t)
                           ("m" "Meeting Notes" entry
                            (file+headline +org-capture-notes-file "Inbox")
                            "* NOTE %u %? :meeting:\n** Present at meeting\n- [ ] \n** Agenda\n** Notes" :prepend t :kill-buffer t)
                           ("N" "Notes" entry
                            (file+headline +org-capture-notes-file "Inbox")
                            "* NOTE %u %?\n%i" :prepend t :kill-buffer t)
                          )
  )
)

;; Setup `Org-clock'
(use-package org-clock
  :ensure nil
  :after org
  :preface
  (defun penguin/org-mode-ask-effort ()
    "Ask for an effort estimate when clocking in."
    (unless (org-entry-get (point) "Effort")
      (let ((effort
             (completing-read
              "Effort: "
              (org-entry-get-multivalued-property (point) "Effort"))))
        (unless (equal effort "")
          (org-set-property "Effort" effort)))))
  :hook (org-clock-in-prepare-hook . penguin/org-mode-ask-effort)
  :custom
  (org-clock-clocktable-default-properties
   '(:block day :maxlevel 2 :scope agenda :link t :compact t :formula %
            :step day :fileskip0 t :stepskip0 t :narrow 80
            :properties ("Effort" "CLOCKSUM" "CLOCKSUM_T" "TODO")))
  (org-clock-continuously nil)
  (org-clock-in-switch-to-state "STARTED")
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-persist t)
  (org-clock-persist-file (expand-file-name (format "%s/emacs/org-clock-save.el" xdg-cache)))
  (org-clock-persist-query-resume nil)
  (org-clock-report-include-clocking-task t)
  (org-show-notification-handler (lambda (msg) (alert msg)))
)

;; ;; Setup `Org-journal'
;; (use-package org-journal
;;   :after org
;;   :bind (("C-c T" . org-journal-new-entry)
;;          ("C-c Y" . journal-file-yesterday))
;;   :preface
;;   (defun get-journal-file-yesterday ()
;;     "Gets filename for yesterday's journal entry."
;;     (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
;;            (daily-name (format-time-string "%Y%m%d" yesterday)))
;;       (expand-file-name (concat org-journal-dir daily-name)))
;;   )
;;   (defun journal-file-yesterday ()
;;     "Creates and load a file based on yesterday's date."
;;     (interactive)
;;     (find-file (get-journal-file-yesterday))
;;   )
;;   :custom
;;   (org-journal-date-format "%e %b %Y (%A)")
;;   (org-journal-dir "~/.emacs.d/.private/org/journal/")
;;   (org-journal-file-type 'weekly)
;;   (org-journal-enable-encryption t)
;;   (org-journal-file-format "%Y%m%d")
;;   (org-journal-time-format "")
;; )


(provide 'setup_org)
;;; setup_org.el ends here
