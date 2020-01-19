;;; setup_lisp.el --- Setting for Lisp coding language
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Setting for Lisp coding language
;; -----------------------------------------------------------

;;; Code:

(use-package ielm
  :ensure nil
  :hook (ielm-mode . (lambda () (setq-local scroll-margin 0)))
)


(provide 'setup_lisp)
;;; setup_lisp.el ends here
