;;; setup_misc.el --- Setting up the constants, ultilities function for further setup
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Setting up the constants, ultilities function for further setup
;; -----------------------------------------------------------

;;; Code:

(defconst nlemacs/version
  "0.0.1"
  "Nghia Lam Emacs version."
)

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
;;; setup_misc.el ends here
