;; setup_flycheck.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up flycheck packages
;; -----------------------------------------------------------


;; Setup `flycheck' - For syntax highlight
(use-package flycheck)
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

;; Addon - Clang format on Save
(use-package clang-format)
(defun clang-format-on-save ()
  (add-hook 'before-save-hook #'clang-format-buffer nil 'local)
)
(add-hook 'c++-mode-hook 'clang-format-on-save)
(add-hook 'c-mode-hook 'clang-format-on-save)

;; Addon - Clang format Google Style on C-f10
(require 'cc-mode)
(defun clang-format-region-google (s e)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point))))
  (clang-format-region s e "Google")
)

(define-key c-mode-base-map (kbd "C-<f10>") #'clang-format-region-google)

(provide 'setup_flycheck)
