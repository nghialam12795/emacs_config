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
  :init
  (progn
    (ido-mode 1)
    (use-package ido-vertical-mode
      :config
      (ido-vertical-mode 1)
      :custom
      (ido-vertical-define-keys 'c-n-and-c-p-only)))
  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-create-new-buffer 'always)
  (ido-default-buffer-method 'selected-window)
)

(use-package ido-completing-read+
  :demand t
  :init
  (ido-ubiquitous-mode 1)
)

(provide 'setup_ido)
;;; setup_ido.el ends here
