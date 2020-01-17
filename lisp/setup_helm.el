;;; setup_helm.el --- Setting up the Helm packages
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; This file is created based on the author's coding habit.
;; This may not be suitable for some people.

;;; Commentary:
;;
;; Setting up the Helm packages
;; -----------------------------------------------------------

;;; Code:

;; Setup `helm'
(use-package helm
  :ensure t
  :config
    (setq helm-M-x-fuzzy-match t
	  helm-mode-fuzzy-match t
	  helm-buffers-fuzzy-matching t
	  helm-recentf-fuzzy-match t
	  helm-locate-fuzzy-match t
	  helm-semantic-fuzzy-match t
	  helm-imenu-fuzzy-match t
	  helm-completion-in-region-fuzzy-match t
	  helm-candidate-number-list 80
	  helm-split-window-inside-p t
	  helm-move-to-line-cycle-in-source t
	  helm-echo-input-in-header-line t
	  helm-autoresize-max-height 0
	  helm-autoresize-min-height 20)
    (helm-mode 1)
)

;; RipGrep
(use-package helm-rg :ensure t)





(provide 'setup_helm)
;;; setup_helm.el ends here
