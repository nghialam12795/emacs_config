;; setup_ui.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up the UI, themes for Emacs
;; -----------------------------------------------------------

;; Setup Themes
(use-package doom-themes
  :defines doom-themes-treemacs-theme
  :functions doom-themes-hide-modeline
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

;; Setup Icons
(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 1.0)
)
(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup)
)

;; Setup Tree Directory
(use-package neotree
  :ensure t
  :init (global-set-key [f8] 'neotree-toggle)
)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq inhibit-compacting-font-caches t) ;; For fixing the lag with all-the-icons 

;; Setup Dashboard
(defcustom e_logo (expand-file-name "res/emacs_logo.png" user-emacs-directory)
  "Set up custom logo for the dashboard."
  :type 'string
)
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner (or e_logo 'official)
        dashboard-banner-logo-title "Nghia Lam's Emacs"
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


(provide 'setup_ui)
