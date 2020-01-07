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


;; -------------------------------------------------------- 
;; Coding Setup
;; -------------------------------------------------------- 

;; ###### Emacs Util ##### ;;
(require 'setup_ido)
(require 'setup_projectile)

;; ######### C++ ######### ;;

;; Package for C++
(require 'setup_rtags)
(require 'setup_flycheck)

;; Style guide
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
