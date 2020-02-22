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
(require 'setup_misc)

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
;; (use-package ivy-posframe
;;   :after ivy
;;   :diminish
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display))
;;         ivy-posframe-height-alist '((t . 20))
;;         ivy-posframe-parameters '((internal-border-width . 10)))
;;   (setq ivy-posframe-width 100)
;; )
;; (ivy-posframe-mode 1)
(use-package ivy-rich
  :init
  (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 35))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
           :predicate
           #'(lambda (cand) (get-buffer cand))
          )
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:width 40 :face font-lock-doc-face))
           )
          )
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 35))
            (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))
          package-install
          (:columns
           ((ivy-rich-candidate (:width 25))
            (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:width 23 :face font-lock-doc-face))))))
  :hook (after-init . ivy-rich-mode)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
)

(use-package swiper
  :after ivy
  :preface
  (defun penguin/swiper ()
    "`swiper' with string returned by `ivy-thing-at-point' as initial input."
    (interactive)
    (swiper (ivy-thing-at-point))
  )
  :bind (("C-s" . penguin/swiper)
         :map swiper-map
         ("C-r" . swiper-query-replace)
        )
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t)
)
(use-package anzu
  :bind
  ([remap query-replace] . anzu-query-replace-regexp)
  :hook
  (after-init . global-anzu-mode)
  :custom
  (anzu-cons-mode-line-p nil)
)

(use-package counsel
  :diminish
  :hook (ivy-mode . counsel-mode)
  :bind (("C-S-s" . helm-projectile-grep))
  :custom
  (counsel-rg-base-command "rg --vimgrep %s")
  :config
  (setq counsel-yank-pop-preselect-last t)
  (setq counsel-yank-pop-separator "\n—————————\n")
  (setq counsel-rg-base-command
        "rg -SHn --no-heading --color never --no-follow --hidden %s"
  )
  (global-set-key (kbd "s-P") #'counsel-M-x)
  (global-set-key (kbd "s-f") #'counsel-grep-or-swiper)
)
(use-package counsel-projectile
  :after ivy
  :config
  (setq-default ivy-initial-inputs-alist nil)
)
(counsel-projectile-mode 1)

;; For tracking history of M-x
(use-package prescient
  :ensure t
  :config
  (setq prescient-history-length 200)
  (setq prescient-save-file (concat pcache-dir "prescient-items"))
  (setq prescient-filter-method '(literal regexp))
  (prescient-persist-mode 1)
)

(use-package ivy-prescient
  :ensure t
  :after (prescient ivy)
  :config
  (setq ivy-prescient-sort-commands
        '(:not counsel-grep
               counsel-rg
               counsel-switch-buffer
               ivy-switch-buffer
               swiper
               swiper-multi))
  (setq ivy-prescient-retain-classic-highlighting t)
  (setq ivy-prescient-enable-filtering nil)
  (setq ivy-prescient-enable-sorting t)
  (ivy-prescient-mode 1)
)

(provide 'setup_ivy)
;;; setup_ivy.el ends here
