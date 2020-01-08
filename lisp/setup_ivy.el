;; setup_ivy.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up Ivy package for a smoother navigation
;; -----------------------------------------------------------


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

(provide 'setup_ivy)
