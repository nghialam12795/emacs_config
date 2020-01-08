;; setup_clangformat.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up the clang format style for C++
;; -----------------------------------------------------------


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

(provide 'setup_clangformat)
