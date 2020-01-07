;; setup_base.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up the basic for Emacs
;; -----------------------------------------------------------


;; UTF-8 support
(prefer-coding-system 'utf-8)

;; Turn off unnecessary 
(custom-set-variables
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
)

;; Turn off annoying sound
(setq visible-bell 1)

;; Make backup to a designated dir, mirroring the full path
(defun my_backup_file_name (fpath)
  (let* (
        (backupRootDir "~/.emacs.d/_backup/")
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
        )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
  )
)
(setq make-backup-file-name-function 'my_backup_file_name)

;; Personal settings will be written to user.el
(setq custom-file (expand-file-name "user.el" user-emacs-directory))

(let ((user-template-file
       (expand-file-name "user_template.el" user-emacs-directory)))
  (if (and (file-exists-p user-template-file)
           (not (file-exists-p custom-file))
      )
      (copy-file user-template-file custom-file)
  )
)

(if (file-exists-p custom-file)
    (load custom-file)
)

(provide 'setup_base)
