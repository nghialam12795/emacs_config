;;; init.el --- init file for emacs

;; ---------------------------------------------------------- ;;
;;                                                            ;;
;;                                                            ;;
;;  _______ _______  ______   _______  ___ ___  ___  ______   ;;
;; |   _   |   _   ||   _  \ |   _   ||   Y   ||   ||   _  \  ;;
;; |.  |   |.  |___||.  |   ||.  |___||.  |   ||.  ||.  |   | ;;
;; |.  ____|.  __)_ |.  |   ||.  |   ||.  |   ||.  ||.  |   | ;;
;; |:  |   |:  |   ||:  |   ||:  I   ||:  I   ||:  ||:  |   | ;;
;; |::.|   |::.. . ||::.|   ||::.. . ||::.. . ||::.||::.|   | ;;
;; '---'   '-------''--- ---''-------''-------''---'`--- ---' ;;
;;                                                            ;;
;;                          Emacs                             ;;
;; ---------------------------------------------------------- ;;

;; Author: Nghia Lam
;;
;; This file is developed based on the author coding habit.
;; This may not be suitable for some people.

;;; Commentary:
;;
;; Init file for Emacs settings
;;

;;; Code:
;; --------------------------------------------------------
;; General Setting
;; --------------------------------------------------------

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

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
(require 'setup_hydra)
(require 'setup_web)
(require 'setup_reader)

;; --------------------------------------------------------
;; Coding Setup
;; --------------------------------------------------------

;; ###### Emacs Util ##### ;;
(require 'setup_keyboard)
(require 'setup_ivy)
(require 'setup_projectile)
(require 'setup_git)
(require 'setup_company)
(require 'setup_org)
(require 'setup_docsets)
(require 'setup_yasnippet)

;; ##### Emacs Lisp ###### ;;
(require 'setup_lisp)

;; END Lisp --------


;; ######### C++ ######### ;;
(require 'setup_lspmode)
(require 'setup_flycheck)
(require 'setup_cmake)
(require 'setup_ccls)

;; Style guide
(require 'setup_clangformat)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; END C++ --------


;; ######## Python  ######## ;;
(require 'setup_python)

;; END Python --------


;; ####### Markdown ####### ;;
(require 'setup_markdown)

;; END Markdown --------



;;; init.el ends here
