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

(custom-set-variables
 '(custom-enabled-themes (quote (doom-molokai)))
 '(custom-safe-themes
   (quote
    ("be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "88a3c267ce2132defd46f2a4761925983dcbc35b1c3cfff1dded164ce169fed4" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "ca849ae0c889eb918785cdc75452b1e11a00848a5128a95a23872e0119ccc8f4" "0809c08440b51a39c77ec5529f89af83ab256a9d48107b088d40098ce322c7d8" "ec8246f6f74bfe0230521412d88092342c17c1c0448a4b8ba39bddd3da170590" "1c8171893a9a0ce55cb7706766e57707787962e43330d7b0b6b0754ed5283cda" "d261bb8f66be37752791a67f03dd24361592ce141b32d83bcbe63ec1c738b087" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "615123f602c56139c8170c153208406bf467804785007cdc11ba73d18c3a248b" "774aa2e67af37a26625f8b8c86f4557edb0bac5426ae061991a7a1a4b1c7e375" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" default))  )
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
                                    :height (cond (sys/macos 130)
                                                  (sys/win32 100)
						  (t 100)
					    )
		)
)

(provide 'setup_ui)
