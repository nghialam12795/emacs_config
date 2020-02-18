;;; setup_otherlang.el --- Quick setup for other languages
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; This file is created based on the author's coding habit.
;; This may not be suitable for some people.

;;; Commentary:
;;
;; Quick setup for other languages
;; -----------------------------------------------------------

;;; Code:

;; `YAML'
(use-package yaml-mode
  :delight "ψ "
  :mode "\\.yml\\'"
  :interpreter ("yml" . yml-mode)
)

;; `XML'
(use-package xml-mode
  :ensure nil
  :mode ("\\.wsdl\\'" "\\.xsd\\'")
)

;; `CSS'
(use-package css-mode
  :ensure nil
  :custom (css-indent-offset 2)
)
(use-package scss-mode
  :ensure nil
  :preface
  (defun me/scss-set-comment-style ()
    (setq-local comment-end "")
    (setq-local comment-start "//"))
  :mode ("\\.sass\\'" "\\.scss\\'")
  :hook (scss-mode . me/scss-set-comment-style)
)

;; `CSV'
(use-package csv-mode
  :config (setq-default csv-align-padding 2)
)

;; `Lua'
(use-package lua-mode
  :delight "Λ "
  :mode "\\.lua\\'"
  :interpreter ("lua" . lua-mode)
)

(provide 'setup_otherlang)
;;; setup_otherlang.el ends here
