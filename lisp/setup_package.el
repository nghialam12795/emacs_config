;; setup_package.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up the package requirement for my workflow
;; -----------------------------------------------------------



;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
)

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
 )

(eval-when-compile
  (require 'use-package)
)

;; Built-in packages
(use-package "startup"
  :ensure nil
  :config (setq inhibit-startup-screen t)
)
(setq browse-url-browser-function 'eww-browse-url)

;; Utilities packages
(require 'devdocs-lookup)
(devdocs-setup)
(global-set-key (kbd "C-h C-p") #'devdocs-lookup-python)
(global-set-key (kbd "C-h C-c") #'devdocs-lookup-cpp)


(provide 'setup_package)
