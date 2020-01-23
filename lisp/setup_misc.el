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
(defconst qutebrowser/win32
  "c:/Program Files/qutebrowser/qutebrowser.exe"
  "The installation path of qutebrowser on Window."
)

(defconst qutebrowser/macos
  "/Applications/qutebrowser.app/Contents/MacOS/qutebrowser"
  "The installation path of qutebrowser on MacOS."
)

;; ----------------------------------
;; `FUNCTIONS'
;; ----------------------------------

(defun penguin-emacs-reload-init-file ()
  "Reload your init.el file without restarting Emacs."
  (interactive)
  (load-file "~/.emacs.d/init.el")
)

(defun penguin-emacs-update-config ()
  "Update Penguin Emacs configuration to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (progn
      (message "Updating Penguin Emacs configuration...")
      (cd dir)
      (shell-command "git pull")
      (message "Load new Penguin Emacs configuration...")
      (penguin-emacs-reload-init-file)
      (message "Update finished.")))
)


(provide 'setup_misc)
;;; setup_misc.el ends here
