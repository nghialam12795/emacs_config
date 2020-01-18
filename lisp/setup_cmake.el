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
(use-package cmake-ide
  :config (cmake-ide-setup)
)

(use-package cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode))
	      '(("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist)
)

(provide 'setup_cmake)
;;; setup_cmake.el ends here
