;;; setup_yasnippet.el --- Setting up Yasnippet Packages
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;;
;; Setting up Yasnippet Packages
;; -----------------------------------------------------------


;;; Code:
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config (yasnippet-snippets-initialize)
)

(use-package yasnippet
  :delight yas-minor-mode " υ"
  :hook (yas-minor-mode . penguin/disable-yas-if-no-snippets)
  :config (yas-global-mode)
  :preface
  (defun penguin/disable-yas-if-no-snippets ()
    (when (and yas-minor-mode (null (yas--get-snippet-tables)))
      (yas-minor-mode -1))
  )
)

(use-package ivy-yasnippet :after yasnippet)
(use-package react-snippets :after yasnippet)

(provide 'setup_yasnippet)
;;; setup_yasnippet.el ends here
