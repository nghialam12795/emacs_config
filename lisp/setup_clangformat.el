;;; setup_clangformat.el --- Setting up clangformat style for C++
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;;  Setting up clangformat style for C++
;; -----------------------------------------------------------

;;; Code:


;; Addon - Clang format on Save
(use-package clang-format)
(defun clang-format-on-save ()
  "Format selected region to clangformat."
  (add-hook 'before-save-hook #'clang-format-buffer nil 'local)
)
(add-hook 'c++-mode-hook 'clang-format-on-save)
(add-hook 'c-mode-hook 'clang-format-on-save)

;; Addon - Clang format Google Style on C-f10
(require 'cc-mode)
(defun clang-format-region-google (s e)
  "Format the selected region."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point))))
  (clang-format-region s e "Google")
)

(define-key c-mode-base-map (kbd "C-<f10>") #'clang-format-region-google)

(provide 'setup_clangformat)
;;; setup_clangformat.el ends here
