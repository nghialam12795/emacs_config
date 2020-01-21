;;; setup_lspmode.el --- Setting language server protocol
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Setting language server protocol
;; -----------------------------------------------------------

;;; Code:

;; `lsp'
(use-package lsp-mode
  :commands lsp
  ;; reformat code and add missing (or remove old) imports
  :hook ((before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :bind (("C-c d" . lsp-describe-thing-at-point)
         ("C-c e n" . flymake-goto-next-error)
         ("C-c e p" . flymake-goto-prev-error)
         ("C-c e r" . lsp-find-references)
         ("C-c e R" . lsp-rename)
         ("C-c e i" . lsp-find-implementation)
         ("C-c e t" . lsp-find-type-definition))
)
(use-package lsp-ui
  ;; flycheck integration & higher level UI modules
  :commands lsp-ui-mode
)
(use-package company-lsp
  ;; company-mode completion
  :commands company-lsp
)
(use-package lsp-treemacs
  ;; project wide overview
  :commands lsp-treemacs-errors-list
)
(use-package helm-lsp
  ;; type completion for xref-aprops
  :commands helm-lsp-workspace-symbol
)
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
)


(provide 'setup_lspmode)
;;; setup_lspmode ends here
