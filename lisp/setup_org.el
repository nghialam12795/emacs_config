;;; setup_org.el --- Setting up org mode
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; This file is created based on the author's coding habit.
;; This may not be suitable for some people.

;;; Commentary:
;;
;; Setting up org mode
;; -----------------------------------------------------------

;;; Code:
(require 'setup_hydra)

;; Setup `Org'
(use-package org
  :ensure nil
  :preface
  (defun penguin/org-src-buffer-name (org-buffer-name language)
    "Construct the buffer name for a source editing buffer. See
`org-src--construct-edit-buffer-name'."
    (format "*%s*" org-buffer-name)
  )
  (defun penguin/org-set-ongoing-hydra-body ()
    (setq penguin/ongoing-hydra-body #'hydra-org/body)
  )
  :bind
  (:map org-mode-map
        ([remap backward-paragraph] . penguin/backward-paragraph-dwim)
        ([remap forward-paragraph] . penguin/forward-paragraph-dwim)
        ("<C-return>" . nil)
        ("<C-S-down>" . nil)
        ("<C-S-up>" . nil)
        ("<M-S-down>" . nil)
        ("<M-S-up>" . nil))
  :hook
  ((org-mode . penguin/org-set-ongoing-hydra-body)
   (org-mode . org-sticky-header-mode)
   (org-mode . toc-org-enable))
  :custom
  (org-descriptive-links nil)
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-src-window-setup 'current-window)
  (org-startup-folded nil)
  (org-startup-truncated nil)
  (org-support-shift-select 'always)
  :config
  (advice-add 'org-src--construct-edit-buffer-name :override #'penguin/org-src-buffer-name)
)

(provide 'setup_org)
;;; setup_org.el ends here
