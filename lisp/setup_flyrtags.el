;; setup_flyrtags.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up flycheck rtags (Mac and linux only)
;; -----------------------------------------------------------

(require 'flycheck) ;; Must setup first
(use-package flycheck-rtags) ;; Support for rtags

(defun flycheck-rtags-setup ()
  "Configure flycheck-rtags."
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil)
)

;; Finalize
(add-hook 'c++-mode-hook 'flycheck-rtags-setup)
(add-hook 'c-mode-hook 'flycheck-rtags-setup)

(provide 'setup_flyrtags)
