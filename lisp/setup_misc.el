;; setup_misc.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up the constants, ultilities function for further setup
;; -----------------------------------------------------------


(defconst sys/win32
  (eq system-type 'windows-nt)
  "Are we using a Window OS?"
)

(defconst sys/linux
  (eq system-type 'gnu/linux)
  "Are we using a GNU/Linux OS?"
)

(defconst sys/macos
  (eq system-type 'darwin)
  "Are we using a Mac OS?"
)

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above."
)

(provide 'setup_misc)
