;;; setup_eshell.el --- Settings eshell package
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Setting up Eshell package
;; -----------------------------------------------------------

;;; Code:

(use-package em-smart
  :defer t
  :config
  (eshell-smart-initialize)
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t)
)

(use-package esh-help
  :ensure t
  :defer t
  :config
  (setup-esh-help-eldoc)
)

(use-package esh-autosuggest
  :ensure t
  :hook (eshell-mode . esh-autosuggest-mode)
)

(use-package eshell-prompt-extras
  :ensure t
  :after esh-opt
  :custom
  (eshell-prompt-function #'epe-theme-dakrone)
)

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  :bind
  ("M-`" . eshell-toggle)
)

(use-package eshell-fringe-status
  :ensure t
  :hook
  (eshell-mode . eshell-fringe-status-mode)
)

(setq eshell-prompt-function
      (lambda ()
        (format "%s %s\n%s%s%s "
                (all-the-icons-octicon "repo")
                (propertize (cdr (shrink-path-prompt default-directory)) 'face `(:foreground "white"))
                (propertize "❯" 'face `(:foreground "#ff79c6"))
                (propertize "❯" 'face `(:foreground "#f1fa8c"))
                (propertize "❯" 'face `(:foreground "#50fa7b"))
        )
      )
)

(setq eshell-hist-ignoredups t)
(setq eshell-cmpl-cycle-completions nil)
(setq eshell-cmpl-ignore-case t)
(setq eshell-ask-to-save-history (quote always))
(setq eshell-prompt-regexp "❯❯❯ ")
(add-hook 'eshell-mode-hook
          '(lambda ()
             (progn
               (define-key eshell-mode-map "\C-a" 'eshell-bol)
               (define-key eshell-mode-map "\C-r" 'counsel-esh-history)
               (define-key eshell-mode-map [up] 'previous-line)
               (define-key eshell-mode-map [down] 'next-line)
               )
           )
)


(provide 'setup_eshell)
;;; setup_eshell.el ends here
