;;; setup_docsets.el --- Settings up docsets package (Mostly for helm-dash)
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Setting up docsets package (Mostly for helm-dash)
;; -----------------------------------------------------------

;;; Code:

(use-package helm-dash)
(use-package counsel-dash)
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (setq-local counsel-dash-docsets '("Emacs Lisp"))
                                   (setq helm-current-buffer (current-buffer))
                                 )
)
(add-hook 'c-mode-common-hook '(lambda ()
                                 (setq-local counsel-dash-docsets '("C++"))
                                 (setq helm-current-buffer (current-buffer))
                               )
)

(global-set-key (kbd "M-h")  'counsel-dash-at-point)

(setq dash-docs-docsets-path "~/.emacs.d/.docsets")
;; (setq dash-docs-browser-func 'eww-browse-url)
(setq counsel-dash-min-length 3)
(setq counsel-dash-candidate-format "%d %n (%t)")
(setq counsel-dash-enable-debugging nil)
(setq counsel-dash-ignored-docsets nil)

(provide 'setup_docsets)
;;; setup_docsets.el ends here
