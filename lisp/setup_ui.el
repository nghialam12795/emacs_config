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

;; Setup cursor highlight
(global-hl-line-mode t)

;; Setup title bar
(setq frame-title-format '("" "%b - Penguin Emacs 🐧"))

;; Setup time, date and battery life
(setq display-time-day-and-date t)
(display-time)

(use-package fancy-battery
  :diminish
  :config
    (setq fancy-battery-show-percentage t)
    (setq battery-update-interval 15)
    (fancy-battery-mode)
    (display-battery-mode)
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

;; `doom-modeline'
(use-package doom-modeline
  :demand t
  :custom
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-enable-word-count t)
  (doom-modeline-icon t)
  (doom-modeline-percent-position nil)
  (doom-modeline-vcs-max-length 28)
  :config
  (doom-modeline-def-segment buffer-default-directory
    "The buffer directory."
    (let* ((active (doom-modeline--active))
           (face (if active 'doom-modeline-buffer-path 'mode-line-inactive)))
      (concat (doom-modeline-spc)
              (propertize (abbreviate-file-name default-directory) 'face face)
              (doom-modeline-spc))))
  (doom-modeline-def-segment penguin/buffer-name
    "The buffer name."
    (concat (doom-modeline-spc) (doom-modeline--buffer-name) (doom-modeline-spc)))
  (doom-modeline-def-segment penguin/buffer-name-simple
    "The buffer name but stimpler."
    (let* ((active (doom-modeline--active))
           (face (cond ((and buffer-file-name (buffer-modified-p)) 'doom-modeline-buffer-modified)
                       (active 'doom-modeline-buffer-file)
                       (t 'mode-line-inactive))))
      (concat (doom-modeline-spc) (propertize "%b" 'face face) (doom-modeline-spc))))
  (doom-modeline-def-segment penguin/buffer-pos
    "The buffer position."
    (let* ((active (doom-modeline--active))
           (face (if active 'mode-line 'mode-line-inactive)))
      (propertize (concat (doom-modeline-spc) (format-mode-line "%l:%c") (doom-modeline-spc))
                  'face face)))
  (doom-modeline-def-segment penguin/major-mode
    "The current major mode, including environment information."
    (let* ((active (doom-modeline--active))
           (face (if active 'doom-modeline-buffxser-major-mode 'mode-line-inactive)))
      (propertize (concat (doom-modeline-spc) mode-name (doom-modeline-spc))
                  'face face)))
  (doom-modeline-def-segment me/vcs
    "The version control system information."
    (when-let ((branch doom-modeline--vcs-text))
      (let ((active (doom-modeline--active))
            (text (concat ":" branch)))
        (concat (doom-modeline-spc)
                (if active text (propertize text 'face 'mode-line-inactive))
                (doom-modeline-spc)))))
  (doom-modeline-mode 1)
  (doom-modeline-def-modeline 'info
    '(bar penguin/buffer-name info-nodes penguin/buffer-pos selection-info)
    '(irc-buffers matches process penguin/major-mode workspace-name))
  (doom-modeline-def-modeline 'main
    '(bar penguin/buffer-name remote-host penguin/buffer-pos checker selection-info)
    '(irc-buffers matches process me/vcs penguin/major-mode workspace-name))
  (doom-modeline-def-modeline 'message
    '(bar penguin/buffer-name-simple penguin/buffer-pos selection-info)
    '(irc-buffers matches process penguin/major-mode workspace-name))
  (doom-modeline-def-modeline 'project
    '(bar buffer-default-directory)
    '(irc-buffers matches process penguin/major-mode workspace-name))
  (doom-modeline-def-modeline 'special
    '(bar penguin/buffer-name penguin/buffer-pos selection-info)
    '(irc-buffers matches process penguin/major-mode workspace-name))
  (doom-modeline-def-modeline 'vcs
    '(bar penguin/buffer-name remote-host penguin/buffer-pos selection-info)
    '(irc-buffers matches process penguin/major-mode workspace-name))
)

(provide 'setup_ui)
;;; setup_ui.el ends here
