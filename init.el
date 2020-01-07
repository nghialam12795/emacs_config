
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
        ("melpa" . "http://melpa.org/packages/"))
)

;; Load path
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "lisp" user-emacs-directory) load-path)
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

;; ###### Emacs Nav ###### ;;
(ido-mode t)
(ido-everywhere t)
(setq ido-use-virtual-buffers t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)

;; ######### C++ ######### ;;

;; Style guide
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
