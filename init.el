;; This is my Emacs Configuration
;;
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
        ("melpa" . "http://melpa.org/packages/")))

;; Load path
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "lisp" user-emacs-directory) load-path)
)
(update-load-path)

;; Setting Interface
(prefer-coding-system 'utf-8)
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("ab2cbf30ab758c5e936b527377d543ce4927001742f79519b62c45ba9dd9f55e" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))

;; Turn off annoying sound
(setq visible-bell 1)

;; -------------------------------------------------------- 
;; Coding Setup
;; -------------------------------------------------------- 

;; ###### Emacs Nav ###### ;;
(ido-mode t)
(ido-everywhere t)

;; ######### C++ ######### ;;

;; Style guide
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
