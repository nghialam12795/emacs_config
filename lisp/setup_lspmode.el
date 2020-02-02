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
         (before-save . lsp-organize-imports)
         (lsp-mode . lsp-enable-which-key-integration)
        )
  :bind (("C-c d" . lsp-describe-thing-at-point)
         ("C-c e n" . flymake-goto-next-error)
         ("C-c e p" . flymake-goto-prev-error)
         ("C-c e r" . lsp-find-references)
         ("C-c e R" . lsp-rename)
         ("C-c e i" . lsp-find-implementation)
         ("C-c e t" . lsp-find-type-definition))
)
(use-package lsp-ui
  :custom-face
  (lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("C-c h l" . hydra-lspui/body)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-enable t
              lsp-ui-doc-use-webkit nil
              lsp-ui-doc-delay 0.2
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'at-point
              lsp-ui-doc-border (face-foreground 'default)
              lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer

              lsp-ui-flycheck-enable t
              
              lsp-ui-imenu-enable t
              lsp-ui-imenu-kind-position 'top
              lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                    ,(face-foreground 'font-lock-string-face)
                                    ,(face-foreground 'font-lock-constant-face)
                                    ,(face-foreground 'font-lock-variable-name-face))

              lsp-ui-sideline-enable nil
              lsp-ui-sideline-ignore-duplicate t
              lsp-ui-sideline-show-symbol t
              lsp-ui-sideline-show-hover t
              lsp-ui-sideline-show-diagnostics nil
              lsp-ui-sideline-show-code-actions t

              lsp-ui-peek-enable t
              lsp-ui-peek-peek-height 20
              lsp-ui-peek-list-width 50
              lsp-ui-peek-fontify 'on-demand ;; never, on-demand, or always
        )
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip))))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
  (defun my-lsp-ui-imenu-hide-mode-line ()
    "Hide the mode-line in lsp-ui-imenu."
    (setq mode-line-format nil))
  (advice-add #'lsp-ui-imenu :after #'my-lsp-ui-imenu-hide-mode-line)
)
(use-package company-lsp
  ;; company-mode completion
  :commands company-lsp
)
(use-package lsp-treemacs
  ;; project wide overview
  :commands lsp-treemacs-errors-list
)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
)
(add-hook 'prog-mode-hook #'lsp)
(provide 'setup_lspmode)
;;; setup_lspmode ends here
