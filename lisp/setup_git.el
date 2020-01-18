;;; setup_git.el --- Settings up git package for working with git
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Setting up Git Package for working with git
;; -----------------------------------------------------------

;;; Code:

(use-package git-commit
  :preface
  (defun me/git-commit-auto-fill-everywhere ()
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil))
  :hook
  (git-commit-mode . me/git-commit-auto-fill-everywhere)
  :custom
  (git-commit-summary-max-length 50)
)
(use-package magit
  :bind
  (:map magit-hunk-section-map
        ("RET" . magit-diff-visit-file-other-window)
        ([return] . magit-diff-visit-file-other-window))
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-highlight-hunk-body nil)
  (magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside magit-diff-highlight-hunk-region-using-face))
  (magit-popup-display-buffer-action '((display-buffer-same-window)))
  (magit-refs-show-commit-count 'all)
  (magit-section-show-child-count t)
  :config
  (remove-hook 'magit-section-highlight-hook #'magit-section-highlight)
)
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)


(provide 'setup_git)
;;; setup_git.el ends here
