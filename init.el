;;
;;███╗░░██╗░██████╗░██╗░░██╗██╗░█████╗░  ██╗░░░░░░█████╗░███╗░░░███╗██╗░██████╗
;;████╗░██║██╔════╝░██║░░██║██║██╔══██╗  ██║░░░░░██╔══██╗████╗░████║╚█║██╔════╝
;;██╔██╗██║██║░░██╗░███████║██║███████║  ██║░░░░░███████║██╔████╔██║░╚╝╚█████╗░
;;██║╚████║██║░░╚██╗██╔══██║██║██╔══██║  ██║░░░░░██╔══██║██║╚██╔╝██║░░░░╚═══██╗
;;██║░╚███║╚██████╔╝██║░░██║██║██║░░██║  ███████╗██║░░██║██║░╚═╝░██║░░░██████╔╝
;;╚═╝░░╚══╝░╚═════╝░╚═╝░░╚═╝╚═╝╚═╝░░╚═╝  ╚══════╝╚═╝░░╚═╝╚═╝░░░░░╚═╝░░░╚═════╝░
;;
;;███████╗███╗░░░███╗░█████╗░░█████╗░░██████╗
;;██╔════╝████╗░████║██╔══██╗██╔══██╗██╔════╝
;;█████╗░░██╔████╔██║███████║██║░░╚═╝╚█████╗░
;;██╔══╝░░██║╚██╔╝██║██╔══██║██║░░██╗░╚═══██╗
;;███████╗██║░╚═╝░██║██║░░██║╚█████╔╝██████╔╝
;;╚══════╝╚═╝░░░░░╚═╝╚═╝░░╚═╝░╚════╝░╚═════╝░


;; -------------------------------------------------------- 
;; General Setting
;; -------------------------------------------------------- 

;; Package archives
(package-initialize)
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/"))
)

;; Load path
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "lisp" user-emacs-directory) load-path)
  (push (expand-file-name "site_lisp" user-emacs-directory) load-path)
)
(update-load-path)

;; Setting Interface
(require 'setup_misc)
(require 'setup_base)
(require 'setup_package)
(require 'setup_ui)
(require 'setup_ido)

;; -------------------------------------------------------- 
;; Coding Setup
;; -------------------------------------------------------- 

;; ###### Emacs Util ##### ;;
(require 'setup_keyboard)
(require 'setup_ivy)
(require 'setup_projectile)

;; ######### C++ ######### ;;

;; Package for C++
(require 'setup_flycheck)
(when (or sys/macos sys/linux)
  (require 'setup_rtags) ;; not support on windows
  (require 'setup_flyrtags)
)
(when sys/win32

)

;; Style guide
(require 'setup_clangformat)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; END C++ --------


;; ####### Markdown ####### ;;
(require 'setup_markdown)

;; END Markdown --------
