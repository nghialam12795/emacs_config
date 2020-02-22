;;; setup_ccls.el --- Settings ccls packages for C++
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Setting up ccls packages for C++
;; -----------------------------------------------------------

;;; Code:
(require 'setup_misc)

;; `ccls'
(use-package ccls
  :after projectile
  :ensure t
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (push ".ccls-cache" projectile-globally-ignored-directories)
)

(require 'lsp-clients)
(setq lsp-disabled-clients '(clangd)) ;; Disable unused clangd server
;; (when sys/macos
;;   (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
;; )

(provide 'setup_ccls)
;;; setup_ccls.el ends here
