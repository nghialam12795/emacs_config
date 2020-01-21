;;; setup_base.el --- Basic settings for Emacs
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Basic settings for Emacs
;; -----------------------------------------------------------

;;; Code:
(setq-default user-full-name "Nghia Lam"
              user-mail-address "nghialam12795@gmail.com"
)

;; Default
(setq-default
  ad-redefinition-action 'accept                   ; Silence warnings for redefinition
  auto-window-vscroll nil                          ; Lighten vertical scroll
  confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
  cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
  delete-by-moving-to-trash t                      ; Delete files to trash
  display-time-default-load-average nil            ; Don't display load average
  display-time-format "%H:%M"                      ; Format the time string
  fill-column 80                                   ; Set width for automatic line breaks
  help-window-select t                             ; Focus new help windows when opened
  indent-tabs-mode nil                             ; Stop using tabs to indent
  inhibit-startup-screen t                         ; Disable start-up screen
  initial-scratch-message ""                       ; Empty the initial *scratch* buffer
  mouse-yank-at-point t                            ; Yank at point rather than pointer
  ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
  recenter-positions '(5 top bottom)               ; Set re-centering positions
  scroll-conservatively most-positive-fixnum       ; Always scroll by one line
  scroll-margin 10                                 ; Add a margin when scrolling vertically
  select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
  sentence-end-double-space nil                    ; End a sentence after a dot and a space
  show-help-function nil                           ; Disable help messages
  show-trailing-whitespace nil                     ; Display trailing whitespaces
  split-height-threshold nil                       ; Disable vertical window splitting
  split-width-threshold nil                        ; Disable horizontal window splitting
  tab-width 4                                      ; Set width for tabs
  uniquify-buffer-name-style 'forward              ; Uniquify buffer names
  window-combination-resize t                      ; Resize windows proportionally
  x-stretch-cursor t)                              ; Stretch cursor to the glyph width
(cd "~/")                                          ; Move to the user directory
(delete-selection-mode 1)                          ; Replace region when inserting text
(display-time-mode 1)                              ; Enable time in the mode-line
(fringe-mode 0)                                    ; Disable fringes
(fset 'yes-or-no-p 'y-or-n-p)                      ; Replace yes/no prompts with y/n
(global-subword-mode 1)                            ; Iterate through CamelCase words
(mouse-avoidance-mode 'banish)                     ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)               ; Enable downcase-region
(put 'upcase-region 'disabled nil)                 ; Enable upcase-region
(set-default-coding-systems 'utf-8)                ; Default to utf-8 encoding
(toggle-frame-maximized)                           ; Toggle maximized

;; Set up cursor
;; (setq-default cursor-type 'bar)
(setq-default cursor-in-non-selected-windows 'hollow)

;; Turn off unnecessary
(custom-set-variables
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(blink-cursor-mode nil)
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


;; personal settings will be written to user.el
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
;;; setup_base.el ends here
