;;; setup_ivy.el --- Setting up the Ivy package for a smoother navigation
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Setting up Ivy package for a smoother navigation
;; -----------------------------------------------------------

;;; Code:


;; Prerequisite
(use-package flx)

;; Setup `Ivy'
(use-package ivy
  :diminish
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-display-style nil)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit)
  (setq ivy-re-builders-alist
        '((counsel-rg . ivy--regex-plus)
          (counsel-projectile-rg . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-projectile-ag . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil)
)
(use-package ivy-posframe
  :after ivy
  :diminish
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-height-alist '((t . 20))
        ivy-posframe-parameters '((internal-border-width . 10)))
  (setq ivy-posframe-width 70)
)
(ivy-posframe-mode 1)
(use-package swiper
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t)
)
(use-package counsel
  :diminish
  :hook (ivy-mode . counsel-mode)
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("s-f" . swiper)
         ("C-S-s" . swiper-all)
	)
  :custom
  (counsel-rg-base-command "rg --vimgrep %s")
  :config
  (global-set-key (kbd "s-P") #'counsel-M-x)
  (global-set-key (kbd "s-f") #'counsel-grep-or-swiper)
)
(use-package counsel-projectile)
(counsel-projectile-mode 1)

(provide 'setup_ivy)
;;; setup_ivy.el ends here
