;;; setup_ui.el --- Setting the ui, themes for Emacs
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;;
;; Setting the ui, themes for Emacs
;; -----------------------------------------------------------

;;; Code:
(require 'setup_misc)

;; ###########################
;; Setup Themes
;; ###########################
(use-package doom-themes
  :defines doom-themes-treemacs-theme
  :config
  (doom-themes-visual-bell-config)
  (set-face-attribute 'doom-visual-bell nil
                      :inherit 'mode-line
                      :background (face-foreground 'error)
                      :inverse-video 'unspecified)
  (doom-themes-org-config)
  (with-eval-after-load 'erc-goodies
    (set-face-attribute 'erc-bold-face nil :bold nil)
  )
  (with-eval-after-load 'flycheck
    (set-face-attribute 'flycheck-error nil :underline `(:color ,(doom-color 'error) :style line))
    (set-face-attribute 'flycheck-info nil :underline `(:color ,(doom-color 'highlight) :style line))
    (set-face-attribute 'flycheck-warning nil :underline `(:color ,(doom-color 'warning) :style line))
  )
  (with-eval-after-load 'isearch
    (set-face-attribute 'isearch nil :background (doom-color 'blue) :foreground (doom-color 'dark-blue))
    (set-face-attribute 'lazy-highlight nil :foreground (doom-color 'blue))
  )
)
(load-theme 'doom-gruvbox t)
;; (use-package solaire-mode
;;   :hook
;;   ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
;;   (minibuffer-setup . solaire-mode-in-minibuffer)
;;   :config
;;   (solaire-mode-swap-bg)
;; )
;; (solaire-global-mode +1)
;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)
;; Narrow/Widen
(use-package fancy-narrow
  :diminish
  :hook (after-init . fancy-narrow-mode)
)
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15
)

;; Setup Fonts
(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :height (cond (sys/macos 110)
                                  (sys/win32 90)
                                  (sys/linux 90)
                                  (t 100)
                            )
)

;; Setup title bar
(setq frame-title-format '("" "%b - Penguin Emacs 🐧")
      icon-title-format frame-title-format
)
;; Setup line number
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'page-break-lines-mode)

;; Setup Icons
(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 1.0)
)
(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup)
)
(use-package all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; ############################
;; Setup Tree Directory
;; ############################

;; ;; `Treemacs'
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               500
          treemacs-no-delete-other-windows       t
          treemacs-missing-project-action        'ask
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-no-png-images                 nil
          treemacs-width                         30)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)
  )
)
(use-package treemacs-projectile
  :after treemacs projectile
)
(use-package treemacs-magit
      :after magit
      :commands treemacs-magit--schedule-update
      :hook ((magit-post-commit
              git-commit-post-finish
              magit-post-stage
              magit-post-unstage)
             . treemacs-magit--schedule-update)
)
(setq doom-themes-treemacs-theme "doom-colors")
(doom-themes-treemacs-config)
;; End `Treemacs'
(setq inhibit-compacting-font-caches t) ;; For fixing the lag with all-the-icons

;; Setup Dashboard
(defcustom e_logo (expand-file-name "res/penmacs_logo.png" user-emacs-directory)
  "Set up custom logo for the dashboard."
  :type 'string
)
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner (or e_logo 'official)
        dashboard-banner-logo-title "Penguin Emacs"
        dashboard-set-navigator t
        dashboard-navigator-buttons
        `(
          (
           (,(when (display-graphic-p)
                (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
            "GithubPage" "Browse Github homepage"
            (lambda (&rest _) (browse-url my-homepage))
           )
           (,(when (display-graphic-p)
                (all-the-icons-octicon "repo-pull" :height 1.1 :v-adjust 0.0))
            "Update P-Emacs" "Update Penguin Emacs"
            (lambda (&rest _) (penguin-emacs-update-config))
           )
           (,(when (display-graphic-p)
               (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
            "Update Package" "Update Installed Package"
            (lambda (&rest _) (auto-package-update-now))
           )
           (,(if (display-graphic-p)
                 (all-the-icons-faicon "question" :height 1.2 :v-adjust -0.1)
               "?")
            "Help" "Help (?/h)"
            (lambda (&rest _) ())
            font-lock-string-face
           )
          )
         )
        dashboard-center-content t
        dashboard-items '((recents  . 5)
                          (projects . 5)
                          (agenda   . 5)
                         )
        dashboard-set-footer nil
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        show-week-agenda-p t
  )
  (add-to-list 'dashboard-item-generators '(packages . dashboard-load-packages))
)
(dashboard-setup-startup-hook)


;; Setup my modeline
;; `Telephone-line'
(use-package telephone-line)
(defface p-red '((t (:foreground "white" :background "red"))) "")
(defface p-orangered '((t (:foreground "white" :background "orange red"))) "")
(defface p-orange '((t (:foreground "white" :background "orange"))) "")
(defface p-gold '((t (:foreground "white" :background "gold"))) "")
(defface p-yellow '((t (:foreground "black" :background "#ffaf00"))) "")
(defface p-chartreuse '((t (:foreground "white" :background "chartreuse"))) "")
(defface p-green '((t (:foreground "black" :background "#afaf00"))) "")
(defface p-sgreen '((t (:foreground "white" :background "spring green"))) "")
(defface p-cyan '((t (:foreground "white" :background "cyan"))) "")
(defface p-blue '((t (:foreground "white" :background "blue"))) "")
(defface p-dmagenta '((t (:foreground "white" :background "#8f3f71"))) "")
(setq telephone-line-faces
      '((red . (p-red . p-red))
        (ored . (p-orangered . p-orangered))
        (orange . (p-orange . p-orange))
        (gold . (p-gold . p-gold))
        (yellow . (p-yellow . p-yellow))
        (chartreuse . (p-chartreuse . p-chartreuse))
        (green . (p-green . p-green))
        (sgreen . (p-sgreen . p-sgreen))
        (cyan . (p-cyan . p-cyan))
        (blue . (p-blue . p-blue))
        (dmagenta . (p-dmagenta . p-dmagenta))
        (evil . telephone-line-evil-face)
        (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
        (nil . (mode-line . mode-line-inactive))
       )
)
(setq telephone-line-lhs
      '((green      . (telephone-line-vc-segment))
        (yellow     . (telephone-line-projectile-segment))
        (nil        . (telephone-line-buffer-segment))
        (nil        . (telephone-line-process-segment))
       )
)
(setq telephone-line-rhs
      '((nil        . (telephone-line-flycheck-segment))
        (dmagenta   . (telephone-line-minions-mode-segment))
        (nil        . (telephone-line-misc-info-segment))
       )
)
(setq telephone-line-separator-extra-padding 3
      telephone-line-height 18
      telephone-line-evil-use-short-tag t
)
(telephone-line-mode 1)

;; end `telephone-line'

;; A minor-mode menu for mode-line
(use-package minions
  :hook (after-init . minions-mode)
  :init (setq minions-mode-line-lighter "❤")
)
(use-package time
  :defer t
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  :config
  (display-time-mode t)
)

;; ##########################
;; `Highlight'
;; ##########################
;; Setup cursor highlight
(use-package hl-line
  ;; Highlights the current line
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil
  )
)

;; Highlight matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t)
  :config
  (with-no-warnings
    (defun display-line-overlay (pos str &optional face)
      "Display line at POS as STR with FACE.
FACE defaults to inheriting from default and highlight."
      (let ((ol (save-excursion
                  (goto-char pos)
                  (make-overlay (line-beginning-position)
                                (line-end-position)))))
        (overlay-put ol 'display str)
        (overlay-put ol 'face
                     (or face '(:inherit highlight)))
        ol))

    (defvar-local show-paren--off-screen-overlay nil)
    (defun show-paren-off-screen (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp show-paren--off-screen-overlay)
        (delete-overlay show-paren--off-screen-overlay))
      ;; check if it's appropriate to show match info,
      (when (and (overlay-buffer show-paren--overlay)
                 (not (or cursor-in-echo-area
                          executing-kbd-macro
                          noninteractive
                          (minibufferp)
                          this-command))
                 (and (not (bobp))
                      (memq (char-syntax (char-before)) '(?\) ?\$)))
                 (= 1 (logand 1 (- (point)
                                   (save-excursion
                                     (forward-char -1)
                                     (skip-syntax-backward "/\\")
                                     (point))))))
        ;; rebind `minibuffer-message' called by
        ;; `blink-matching-open' to handle the overlay display
        (cl-letf (((symbol-function #'minibuffer-message)
                   (lambda (msg &rest args)
                     (let ((msg (apply #'format-message msg args)))
                       (setq show-paren--off-screen-overlay
                             (display-line-overlay
                              (window-start) msg ))))))
          (blink-matching-open))))
    (advice-add #'show-paren-function :after #'show-paren-off-screen))
)

(provide 'setup_ui)
;;; setup_ui.el ends here
