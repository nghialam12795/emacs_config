;;; setup_ido.el --- Setting up Ido searching systems
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; This file is created based on the author's coding habit.
;; This may not be suitable for some people.

;;; Commentary:
;;
;; Setting up Ido searching systems
;; -----------------------------------------------------------

;;; Code:

;; Enable ido mode
(use-package ido
  :demand t
  :commands ido-mode
  :config
  (progn
    (ido-mode 1)
    (setq ido-auto-merge-work-directories-length -1
          ido-default-buffer-method 'selected-window
          ido-use-virtual-buffers t
          ido-use-filename-at-point nil
          ido-create-new-buffer 'always)
    (setq ido-enable-flex-matching t)
    (use-package flx-ido
      :defer 5
      :config
      (progn
        ;; disable ido faces to see flx highlights.
        ;; This makes flx-ido much faster.
        (setq gc-cons-threshold 20000000)
        (flx-ido-mode 1)
        (setq ido-use-faces nil)))
    (use-package ido-vertical-mode
      :config
      (progn
        (ido-vertical-mode 1)
        (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)))
    (use-package flx-ido)
  )
)

(use-package ido-completing-read+
  :demand t
  :init
  (ido-ubiquitous-mode 1)
)

(provide 'setup_ido)
;;; setup_ido.el ends here
