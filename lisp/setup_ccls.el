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
  ;; :init
  ;; (setq ccls-initialization-options '(:clang (:extraArgs
  ;;                                             ["-isysroot"
  ;;                                              "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"
  ;;                                             ]
  ;;                                            )
  ;;                                    )
  ;; )
  :custom
  (ccls-sem-highlight-method 'font-lock)
  :config
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))
        )
)
(require 'lsp-clients)
(setq ccls-executable (cond (sys/macos "/usr/local/Cellar/ccls/0.20190823.5/bin/ccls")
                            (sys/win32 "temp")
                            (sys/linux "/usr/local/bin/ccls")
                      )
)
(setq lsp-disabled-clients '(clangd)) ;; Disable unused clangd server
;; (when sys/macos
;;   (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
;; )

(provide 'setup_ccls)
;;; setup_ccls.el ends here
