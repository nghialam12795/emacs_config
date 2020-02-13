;;; setup_projectile.el --- Setting up the Projectile packages
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; This file is created based on the author's coding habit.
;; This may not be suitable for some people.

;;; Commentary:
;;
;; Setting up the Projectile packages
;; -----------------------------------------------------------

;;; Code:
(require 'setup_misc)

;; Setup `projectile' - For Project Management
(setq projectile-known-projects-file (concat pcache-dir "projectile.projects"))

(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
         ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
         ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix " "
        projectile-sort-order 'recentf
        projectile-use-git-grep t)
  :config
  
  ;; Helm support
  (projectile-mode)
  (use-package helm-projectile
    :ensure t
  )
  (setq projectile-completion-system 'helm)
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window))
  (helm-projectile-on)
  (use-package wgrep
    :ensure t
    :config
    (use-package wgrep-helm :ensure t)
    (setq wgrep-auto-save-buffer t)
    (setq wgrep-change-readonly-file t)
  )
  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd)))
   )
  ;; Faster searching on Windows
  (when sys/win32
    (setq projectile-indexing-method 'alien)
    (when (or (executable-find "fd") (executable-find "rg"))
      (setq projectile-indexing-method 'alien
            projectile-enable-caching nil))
    (setq projectile-git-submodule-command nil)
  )

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val)
  )
)

(setq projectile-cache-file (concat pcache-dir "projectile.cache")
      projectile-enable-caching (not noninteractive)
      projectile-indexing-method 'alien
      projectile-known-projects-file (concat pcache-dir "projectile.projects")
      projectile-require-project-root nil
      projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
      projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
)

(provide 'setup_projectile)
;;; setup_projectile.el ends here
