;; setup_rtags.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up Rtags packages for C++ development
;; -----------------------------------------------------------


;; TODO: Need to build Rtags to find rdm & rc
;; Setup `Rtags' - For C++ code completetion
(use-package rtags
  :init
  :ensure t
)
(use-package helm-rtags
  :init (setq rtags-use-helm t)
) ;; Front end for rtags
(use-package company-rtags) ;; Back end for rtags


(rtags-enable-standard-keybindings)
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)

(defun company-rtags-setup ()
  "Configure company-backends for company-rtags."
  (delete 'company-semantic company-backends)
  (setq rtags-completions-enabled t)
  (push '(company-rtags :with company-yasnippet) company-backends)
)

;; Finalize Setup
(rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'company-rtags-setup)
(add-hook 'c-mode-hook 'company-rtags-setup)

(provide 'setup_rtags)
