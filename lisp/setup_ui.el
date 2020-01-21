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
)
(load-theme 'doom-gruvbox t)
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

;; Setup title bar
(setq frame-title-format '("" "%b - Penguin Emacs 🐧"))
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
;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay      0.5
;;           treemacs-directory-name-transformer    #'identity
;;           treemacs-display-in-side-window        t
;;           treemacs-eldoc-display                 t
;;           treemacs-file-event-delay              5000
;;           treemacs-file-extension-regex          treemacs-last-period-regex-value
;;           treemacs-file-follow-delay             0.2
;;           treemacs-file-name-transformer         #'identity
;;           treemacs-follow-after-init             t
;;           treemacs-git-command-pipe              ""
;;           treemacs-goto-tag-strategy             'refetch-index
;;           treemacs-indentation                   2
;;           treemacs-indentation-string            " "
;;           treemacs-is-never-other-window         nil
;;           treemacs-max-git-entries               500
;;           treemacs-missing-project-action        'ask
;;           treemacs-no-delete-other-windows       t
;;           treemacs-project-follow-cleanup        nil
;;           treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                      'left
;;           treemacs-recenter-distance             0.1
;;           treemacs-recenter-after-file-follow    nil
;;           treemacs-recenter-after-tag-follow     nil
;;           treemacs-recenter-after-project-jump   'always
;;           treemacs-recenter-after-project-expand 'on-distance
;;           treemacs-show-cursor                   nil
;;           treemacs-show-hidden-files             t
;;           treemacs-silent-filewatch              nil
;;           treemacs-silent-refresh                nil
;;           treemacs-sorting                       'alphabetic-asc
;;           treemacs-space-between-root-nodes      t
;;           treemacs-tag-follow-cleanup            t
;;           treemacs-tag-follow-delay              1.5
;;           treemacs-no-png-images                 nil
;;           treemacs-width                         30)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode t)
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple))))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)
;;   )
;; )
;; (use-package treemacs-projectile
;;   :after treemacs projectile
;; )
;; (use-package treemacs-magit
;;   :after treemacs magit
;; )
;; (doom-themes-treemacs-config)
;; End `Treemacs'
(setq inhibit-compacting-font-caches t) ;; For fixing the lag with all-the-icons
;; `Neotree'
(use-package neotree
  :config
  ;; modified version of https://github.com/hemmvm/dotemacs/blob/master/site-lisp/util--neotree.el
  (defun neotree-project-tree-open ()
    (interactive)
    (let ((project-dir (ignore-errors (projectile-project-root)))
          (file-name (buffer-file-name)))
      (if project-dir
          (progn
            (neotree-dir project-dir)
            (neotree-find file-name))
        (neotree-find)))
    (neo-global--select-window))
  
  (defun neotree-project-tree-toggle ()
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (neotree-project-tree-open)))
  
  (global-set-key [f8] 'neotree-project-tree-toggle)
  
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 35)
  
  ;; https://github.com/jaypei/emacs-neotree/issues/77 + https://github.com/hemmvm/dotemacs/blob/master/site-lisp/util--neotree.el
  (defun custom-neotree-enter-hide ()
    (interactive)
    (neotree-enter)
    (let ((current (neo-buffer--get-filename-current-line)))
      (if (not (and current (file-accessible-directory-p current)))
          (neotree-hide)))
    )
  
  (defun custom-neotree-peek ()
    (interactive)
    (let ((neo-window (neo-global--get-window)))
      (neotree-enter)
      (select-window neo-window))
  )
  
  (add-hook
   'neotree-mode-hook
   (lambda ()
     (define-key neotree-mode-map (kbd "RET") 'custom-neotree-enter-hide)
   )
  )
  
  (add-hook
   'neotree-mode-hook
   (lambda ()
     (define-key neotree-mode-map (kbd "TAB") 'custom-neotree-peek))
  )
)
;; end `Neotree'

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
               (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
            "Update" "Update Penguin Emacs"
            (lambda (&rest _) (package-refresh-contents))
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

;; `doom-modeline'
(use-package doom-modeline
  :demand t
  :custom
  (doom-modeline-override-battery-modeline t)
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
              (doom-modeline-spc)))
  )
  (doom-modeline-def-segment penguin/buffer-name
    "The buffer name."
    (concat (doom-modeline-spc) (doom-modeline--buffer-name) (doom-modeline-spc))
  )
  (doom-modeline-def-segment penguin/battery-life
    "The buffer name."
    (concat (doom-modeline-spc) (fancy-battery-default-mode-line) (doom-modeline-spc))
  )
  (doom-modeline-def-segment penguin/time
    "Time"
    (when (doom-modeline--active)
      (propertize
       (format-time-string " %b %d, %Y - %H:%M ")
       'face (when (doom-modeline--active) `(:foreground "#1b335f" :background "#edb672"))))
  )
  (doom-modeline-def-segment penguin/buffer-name-simple
    "The buffer name but stimpler."
    (let* ((active (doom-modeline--active))
           (face (cond ((and buffer-file-name (buffer-modified-p)) 'doom-modeline-buffer-modified)
                       (active 'doom-modeline-buffer-file)
                       (t 'mode-line-inactive))))
      (concat (doom-modeline-spc) (propertize "%b" 'face face) (doom-modeline-spc)))
  )
  (doom-modeline-def-segment penguin/buffer-pos
    "The buffer position."
    (let* ((active (doom-modeline--active))
           (face (if active 'mode-line 'mode-line-inactive)))
      (propertize (concat (doom-modeline-spc) (format-mode-line "%l:%c") (doom-modeline-spc))
                  'face face))
  )
  (doom-modeline-def-segment penguin/vsc
    "The version control system information."
    (when-let ((branch doom-modeline--vcs-text))
      (let ((active (doom-modeline--active))
            (text (concat ":" branch)))
        (concat (doom-modeline-spc)
                (if active text (propertize text 'face 'mode-line-inactive))
                (doom-modeline-spc))))
  )
  (doom-modeline-mode 1)
  (doom-modeline-def-modeline 'info
    '(bar penguin/buffer-name info-nodes penguin/buffer-pos selection-info )
    '(irc-buffers matches process major-mode workspace-name penguin/battery-life penguin/time))
  (doom-modeline-def-modeline 'main
    '(bar penguin/buffer-name remote-host penguin/buffer-pos checker selection-info )
    '(irc-buffers matches process penguin/vsc major-mode workspace-name penguin/battery-life penguin/time))
  (doom-modeline-def-modeline 'message
    '(bar penguin/buffer-name-simple penguin/buffer-pos selection-info )
    '(irc-buffers matches process major-mode workspace-name penguin/battery-life penguin/time))
  (doom-modeline-def-modeline 'project
    '(bar buffer-default-directory)
    '(irc-buffers matches process major-mode workspace-name penguin/battery-life penguin/time))
  (doom-modeline-def-modeline 'special
    '(bar penguin/buffer-name penguin/buffer-pos selection-info )
    '(irc-buffers matches process major-mode workspace-name penguin/battery-life penguin/time))
  (doom-modeline-def-modeline 'vcs
    '(bar penguin/buffer-name remote-host penguin/buffer-pos selection-info)
    '(irc-buffers matches process major-mode workspace-name penguin/battery-life penguin/time))
)
;; A minor-mode menu for mode-line
(use-package minions
  :hook (after-init . minions-mode)
  :init (setq minions-mode-line-lighter "✬")
)
(use-package fancy-battery
  :after doom-modeline
  :diminish
  :hook (after-init . fancy-battery-mode)
  :config
    (setq fancy-battery-show-percentage t)
    (setq battery-update-interval 15)
    (fancy-battery-mode)
    (display-battery-mode)
)

;; ##########################
;; `Highlight'
;; ##########################
(use-package rainbow-mode
  :delight
  :hook (prog-mode)
)
;; Setup cursor highlight
(global-hl-line-mode t)
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
