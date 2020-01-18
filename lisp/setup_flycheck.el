;;; setup_flycheck.el --- Setting up Flycheck package for linter
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;;  Setting up Flycheck package for linter
;; -----------------------------------------------------------

;;; Code:


;; Setup `flycheck' - For syntax highlight
(use-package flycheck
  :diminish
  :hook ((css-mode . flycheck-mode)
	 (emacs-lisp-mode . flycheck-mode)
	 (python-mode . flycheck-mode)
	 (after-init . global-flycheck-mode)
	)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Set fringe style
  (setq flycheck-indication-mode 'right-fringe)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (if emacs/>=26p
          (use-package flycheck-posframe
            :custom-face (flycheck-posframe-border-face ((t (:inherit default))))
            :hook (flycheck-mode . flycheck-posframe-mode)
            :init (setq flycheck-posframe-border-width 1
                        flycheck-posframe-inhibit-functions
                        '((lambda (&rest _) (bound-and-true-p company-backend)))
		  )
	  )
        (use-package flycheck-pos-tip
          :defines flycheck-pos-tip-timeout
          :hook (global-flycheck-mode . flycheck-pos-tip-mode)
          :config (setq flycheck-pos-tip-timeout 30)
	)
      )
    (use-package flycheck-popup-tip
      :hook (flycheck-mode . flycheck-popup-tip-mode)
    )
  )
)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

(provide 'setup_flycheck)
;;; setup_flycheck.el ends here
