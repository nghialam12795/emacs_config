;;; init.el --- init file for emacs

;; ╭ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ╮
;; |  _______ _______ ______  ___ ___ _______ _______ _______  |
;; | |   _   |   _   |   _  \|   Y   |   _   |   _   |   _   | |
;; | |.  1   |.  1___|.  |   |.      |.  1   |.  1___|   1___| |
;; | |.  ____|.  __)_|.  |   |. \_/  |.  _   |.  |___|____   | |
;; | |:  |   |:  1   |:  |   |:  |   |:  |   |:  1   |:  1   | |
;; | |::.|   |::.. . |::.|   |::.|:. |::.|:. |::.. . |::.. . | |
;; | '---'   `-------`--- ---`--- ---`--- ---'-------'-------' |
;; ╰ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ╯

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
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Load external path
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "external" user-emacs-directory) load-path))

(update-load-path)

;; Load config file
(if (file-exists-p (expand-file-name "config.el" user-emacs-directory))
    (load-file (expand-file-name "config.el" user-emacs-directory))
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory)))


;;; init.el ends here
