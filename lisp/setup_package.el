;;; setup_package.el --- Setting up Use Package for manage packages and configure some minimal packages
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;;
;; Setting up Use Package for manage packages and configure some minimal packages
;; -----------------------------------------------------------

;;; Code:
(require 'setup_misc)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
)
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
)
(eval-when-compile
  (require 'use-package)
)
(use-package gcmh
  :ensure t
  :init
  (gcmh-mode 1)
)
(use-package quelpa
  :ensure t
  :defer t
  :custom
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo.")
)
(use-package quelpa-use-package :ensure t)
;; Package Manager
(use-package paradox
  :custom
  (paradox-column-width-package 27)
  (paradox-column-width-version 13)
  (paradox-execute-asynchronously t)
  (paradox-github-token t)
  (paradox-hide-wiki-packages t)
  :config
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print)
)

;; Built-in packages
(use-package "startup"
  :ensure nil
  :config (setq inhibit-startup-screen t)
)
(use-package dired
  :ensure nil
  :preface
  (defun penguin/dired-directories-first ()
    "Sort dired listings with directories first before adding marks."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2)
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always)
  :config
  (advice-add 'dired-readin :after #'penguin/dired-directories-first)
)
(use-package dired-subtree
  :bind (:map dired-mode-map
              ("<backtab>" . dired-subtree-cycle)
              ("<tab>" . dired-subtree-toggle))
)
(use-package dired-git-info
  :ensure t
  :after dired
  :config
  (setq dgi-commit-message-format "%h\t%s\t%cr")
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)
        )
)
(use-package ibuffer
  :ensure nil
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-auto-mode-match?
              all-the-icons-faicon
              my-ibuffer-find-file)
  :commands (ibuffer-find-file
             ibuffer-current-buffer)
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  (setq ibuffer-saved-filter-groups
        '(("Main"
           ("Directories" (mode . dired-mode))
           ("Org" (mode . org-mode))
           ("Programming" (or
                           (mode . c-mode)
                           (mode . conf-mode)
                           (mode . css-mode)
                           (mode . emacs-lisp-mode)
                           (mode . html-mode)
                           (mode . mhtml-mode)
                           (mode . python-mode)
                           (mode . ruby-mode)
                           (mode . scss-mode)
                           (mode . shell-script-mode)
                           (mode . yaml-mode))
           )
           ("Markdown" (mode . markdown-mode))
           ("Magit" (or
                     (mode . magit-blame-mode)
                     (mode . magit-cherry-mode)
                     (mode . magit-diff-mode)
                     (mode . magit-log-mode)
                     (mode . magit-process-mode)
                     (mode . magit-status-mode))
           )
           ("Apps" (or
                    (mode . bongo-playlist-mode)
                    (mode . mu4e-compose-mode)
                    (mode . mu4e-headers-mode)
                    (mode . mu4e-main-mode)
                    (mode . elfeed-search-mode)
                    (mode . elfeed-show-mode)
                    (mode . mu4e-view-mode))
           )
           ("Emacs" (or
                     (name . "^\\*Help\\*$")
                     (name . "^\\*Custom.*")
                     (name . "^\\*Org Agenda\\*$")
                     (name . "^\\*info\\*$")
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Backtrace\\*$")
                     (name . "^\\*Messages\\*$"))
           )
          )
         )
  )
  ;; Display buffer icons on GUI
  (when (and (display-graphic-p)
             (require 'all-the-icons nil t))
    ;; For alignment, the size of the name field should be the width of an icon
    (define-ibuffer-column icon (:name "  ")
      (let ((icon (if (and (buffer-file-name)
                           (all-the-icons-auto-mode-match?))
                      (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                    (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
          icon)))

    (setq ibuffer-formats `((mark modified read-only ,(if emacs/>=26p 'locked "")
                                  ;; Here you may adjust by replacing :right with :center or :left
                                  ;; According to taste, if you want the icon further from the name
                                  " " (icon 2 2 :left :elide)
                                  ,(propertize " " 'display `(space :align-to 8))
                                  (name 18 18 :left :elide)
                                  " " (size 9 -1 :right)
                                  " " (mode 16 16 :left :elide) " " filename-and-process)
                            (mark " " (name 16 -1) " " filename))))

  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file ()
      (interactive)
      (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                 (if (buffer-live-p buf)
                                     (with-current-buffer buf
                                       default-directory)
                                   default-directory))))
        (counsel-find-file default-directory)))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))

  ;; Group ibuffer's list by project root
  (use-package ibuffer-projectile
    :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
    :hook ((ibuffer . (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic)))))
    :config
    (setq ibuffer-projectile-prefix
          (if (display-graphic-p)
              (concat
               (all-the-icons-octicon "file-directory"
                                      :face ibuffer-filter-group-name-face
                                      :v-adjust -0.05
                                      :height 1.25)
               " ")
            "Project: ")
    )
  )
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "Main"))
  )
)
(use-package ibuffer-vc
  :defer t
  :ensure t
  :config
  (define-ibuffer-column icon
    (:name "Icon" :inline t)
    (all-the-icons-ivy--icon-for-mode major-mode))
  :custom
  (ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           filename-and-process)) "include vc status info")
  :hook
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)
               )
             )
  )
)


;; ###############################
;; Utilities packages
;; ###############################

;; `Calendar'
(use-package calfw
  :commands cfw:open-calendar-buffer
  :bind ("<C-f11>" . open-calendar)
  :init
  (use-package calfw-org
    :commands (cfw:open-org-calendar cfw:org-create-source))

  (use-package calfw-ical
    :commands (cfw:open-ical-calendar cfw:ical-create-source))

  (defun open-calendar ()
    "Open calendar."
    (interactive)
    (unless (ignore-errors
              (cfw:open-calendar-buffer
               :contents-sources
               (list
                (when org-agenda-files
                  (cfw:org-create-source "YellowGreen"))
                (when (bound-and-true-p centaur-ical)
                  (cfw:ical-create-source "gcal" centaur-ical "IndianRed")))))
      (cfw:open-calendar-buffer)
    )
  )
)

;; Clean up whitespaces
(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup)
)
;; imenu-list
;; (use-package imenu-list
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq imenu-list-position 'right
;;         imenu-list-auto-resize t
;;         imenu-list-focus-after-activation t)
;;   :bind
;;   ("<f12>" . imenu-list-smart-toggle)
;; )

;; Fast scrolling (Mac slowness issue)
(use-package fast-scroll
  :ensure t
  :diminish fast-scroll-mode
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1)
)

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode)
)

;; Windows tiling manager
(use-package eyebrowse
  :bind
  ("<f5>" . eyebrowse-switch-to-window-config-1)
  ("<f6>" . eyebrowse-switch-to-window-config-2)
  ("<f7>" . eyebrowse-switch-to-window-config-3)
  ("<f8>" . eyebrowse-switch-to-window-config-4)
  :hook
  (after-init . eyebrowse-mode)
  :custom
  (eyebrowse-new-workspace t)
)

;; Automatic paren
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
)

;; Uniquify buffers name
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-separator "|")
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t)
)

;; Generate a fast and quality graphic
(use-package gnuplot
  :defer 2
)

(use-package gnuplot-mode
  :after gnuplot
  :mode "\\.gp\\'"
)


;; OTHERS -------------------------------------------------------
(use-package copyit)                    ; copy path, url, etc.
(use-package daemons)                   ; system services/daemons
(use-package diffview)                  ; side-by-side diff view
(use-package esup)                      ; Emacs startup profiler
(use-package focus)                     ; Focus on the current region
(use-package list-environment)
(use-package memory-usage)


(provide 'setup_package)
;;; setup_package.el ends here
