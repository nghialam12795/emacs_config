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

;; Setup Themes
(use-package doom-themes
  :defines doom-themes-treemacs-theme
  :config
  (doom-themes-visual-bell-config)
  (set-face-attribute 'doom-visual-bell nil
                      :inherit 'mode-line
                      :background (face-foreground 'error)
                      :inverse-video 'unspecified)
  (doom-themes-org-config)
)

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode)
;; )


;; Setup Fonts
(defun is_font (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name))
)

(cl-loop for font in '("SF Mono" "Hack" "Source Code Pro" "Fira Code"
		       "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
	 when (is_font font)
         return (set-face-attribute 'default nil
                                    :font font
                                    :height (cond (sys/macos 110)
                                                  (sys/win32 100)
  						  (sys/linux 90)
						  (t 100)
					    )
		)
)

;; Setup Icons
(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 1.0)
)
(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup)
)

;; Setup Tree Directory
;; `Treemacs'
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
          treemacs-missing-project-action        'ask
          treemacs-no-delete-other-windows       t
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
          treemacs-width                         30
	  treemacs-resize-icons                  5)

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
  :ensure t
)
;; (use-package treemacs-icons-dired
;;   :after treemacs dired
;;   :ensure t
;;   :config (treemacs-icons-dired-mode)
;; )
;; (use-package treemacs-magit
;;   :after treemacs magit
;;   :ensure t
;; )
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
        dashboard-items '((recents  . 5)
			  (projects . 5)
			  (agenda   . 5)
			 )
        dashboard-set-footer nil
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	show-week-agenda-p t)
)
(dashboard-setup-startup-hook)


;; Setup my modeline
(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (progn
    (spaceline-define-segment buffer-id
      (if (buffer-file-name)
          (let ((project-root (projectile-project-p)))
            (if project-root
                (file-relative-name (buffer-file-name) project-root)
              (abbreviate-file-name (buffer-file-name))))
        (powerline-buffer-id)))
    (setq powerline-default-separator 'contour)
    (spaceline-helm-mode)
    (spaceline-spacemacs-theme)
    (spaceline-toggle-minor-modes-off))
)

(provide 'setup_ui)
;;; setup_ui.el ends here
