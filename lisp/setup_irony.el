;; setup_irony.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up Irony package for C++
;; -----------------------------------------------------------


;; Setup `Irony'
(use-package irony
  :ensure t
  :config
  (use-package company-irony
    :ensure t
    :config
    (add-to-list 'company-backends 'company-irony)
  )
  (use-package company-irony-c-headers
    :ensure t
    :config
    (add-to-list 'company-backends 'company-irony-c-headers)
  )
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async)
  )
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  ;; Windows performance tweaks
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0)
  )
  ;; Set the buffer size to 64K on Windows (from the original 4K)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024))
  )
)

(use-package company
  :ensure t
  :init (global-company-mode)
  :bind (("<backtab>" . company-complete-common-or-cycle))
  ;; :config
  ;; (delete 'company-backends 'company-clang)
)

;; Intergrate with flycheck
(use-package flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
)

(provide 'setup_irony)
