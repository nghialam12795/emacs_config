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

(defconst pemacs/version
  "0.0.1"
  "Penguin Emacs version."
)

(defconst my-homepage
  "https://github.com/nghialam12795"
  "My personal Github page."
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

;; ----------------------------------
(defconst qutebrowser/win
  "c:/Program Files/qutebrowser/qutebrowser.exe"
  "The installation path of qutebrowser on Window."
)

(provide 'setup_misc)
;;; setup_misc.el ends here
