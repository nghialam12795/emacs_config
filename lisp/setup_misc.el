;; setup_misc.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up the constants, ultilities function for further setup
;; -----------------------------------------------------------


(defconst sys/win32
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?"
)

(defconst sys/linux
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?"
)

(defconst sys/macos
  (eq system-type 'darwin)
  "Are we running on a Mac system?"
)

(provide 'setup_misc)
