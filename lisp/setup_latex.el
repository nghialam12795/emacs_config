;;; setup_latex.el --- Setting up Latex document support
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; This file is created based on the author's coding habit.
;; This may not be suitable for some people.

;;; Commentary:
;;
;; Setting up Latex documents support
;; -----------------------------------------------------------

;;; Code:
(use-package tex
  :ensure auctex
  :bind (:map TeX-mode-map
              ("C-c C-o" . TeX-recenter-output-buffer)
              ("C-c C-l" . TeX-next-error)
              ("M-[" . outline-previous-heading)
              ("M-]" . outline-next-heading))
  :hook (LaTeX-mode . reftex-mode)
  :preface
  (defun penguin/switch-to-help-window (&optional ARG REPARSE)
    "Switches to the *TeX Help* buffer after compilation."
    (other-window 1))
  :custom
  (TeX-auto-save t)
  (TeX-byte-compile t)
  (TeX-clean-confirm nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (TeX-source-correlate-mode t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  :config
  (advice-add 'TeX-next-error :after #'penguin/switch-to-help-window)
  (advice-add 'TeX-recenter-output-buffer :after #'penguin/switch-to-help-window)
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
)

(use-package bibtex
  :after auctex
  :hook (bibtex-mode . penguin/bibtex-fill-column)
  :preface
  (defun penguin/bibtex-fill-column ()
    "Ensures that each entry does not exceed 120 characters."
    (setq fill-column 120))
)

(use-package company-auctex
  :after (auctex company)
  :config (company-auctex-init)
)

(use-package company-math :after (auctex company))

(use-package reftex
  :after auctex
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-save-parse-info t)
  (reftex-use-multiple-selection-buffers t)
)

(provide 'setup_latex)
;;; setup_latex.el ends here
