;;; setup_web.el --- Setting up Packages for browse the web with Emacs
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;;
;; Setting up Packages to browse the web with Emacs
;; -----------------------------------------------------------

;;; Code:
(require 'setup_misc)

;; `Browser'
(use-package browse-url
  :ensure nil
  :custom
  ;; (browse-url-browser-function 'eww-browse-url)
  (browse-url-browser-function 'browse-url-generic)
  :config
  (cond (sys/win32 (setq browse-url-generic-program qutebrowser/win32))
        (sys/macos (setq browse-url-generic-program qutebrowser/macos))
        (sys/linux (setq browse-url-generic-program "qutebrowser"))
  )
)

(use-package engine-mode
  :defer 3
  :config
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "i")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m"
    :docstring "Mappin' it up.")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")
  (engine-mode t)
)

;; Handle `HTML'
(use-package htmlize
  :defer t
)

(provide 'setup_web)
;;; setup_web.el ends here
