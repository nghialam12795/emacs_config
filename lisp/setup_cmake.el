;;; setup_cmake.el --- Setting the cmake packages for C++
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Setting the cmake packages for C++
;; -----------------------------------------------------------

;;; Code:

;; Setup `CMake-ide'
(use-package cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode))
	      '(("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist)
)
(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate)
)
(use-package cmake-ide
  :after projectile
  :hook (c++-mode . penguin/cmake-ide-find-project)
  :preface
  (defun penguin/cmake-ide-find-project ()
    "Finds the directory of the project for cmake-ide."
    (with-eval-after-load 'projectile
      (setq cmake-ide-project-dir (projectile-project-root))
      (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
    (setq cmake-ide-compile-command
          (concat "cd " cmake-ide-build-dir " && cmake .. && make"))
    (cmake-ide-load-db))

  (defun penguin/switch-to-compilation-window ()
    "Switches to the *compilation* buffer after compilation."
    (other-window 1))
  :bind ([remap comment-region] . cmake-ide-compile)
  :init (cmake-ide-setup)
  :config (advice-add 'cmake-ide-compile :after #'penguin/switch-to-compilation-window)
)
(provide 'setup_cmake)
;;; setup_cmake.el ends here
