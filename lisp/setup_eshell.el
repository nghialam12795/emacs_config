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




(provide 'setup_eshell)
;;; setup_eshell.el ends here
