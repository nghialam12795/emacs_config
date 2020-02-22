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
(require 'setup_misc)

(setq-default user-full-name "Nghia Lam"
              user-mail-address "nghialam12795@gmail.com"
)

;; Default
(advice-add #'display-startup-echo-area-message :override #'ignore)
(setq-default
  ad-redefinition-action 'accept                   ; Silence warnings for redefinition
  auto-window-vscroll nil                          ; Lighten vertical scroll
  blink-matching-paren nil                         ; Dont blink mathcing paren
  confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
  cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
  compilation-always-kill t                        ; Kill compilation process before starting another
  compilation-ask-about-save nil                   ; Save all buffers on `compile'
  compilation-scroll-output t
  delete-by-moving-to-trash t                      ; Delete files to trash
  display-time-default-load-average nil            ; Don't display load average
  display-time-format "%H:%M"                      ; Format the time string
  fill-column 80                                   ; Set width for automatic line breaks
  fringe-indicator-alist
    (delq (assq 'continuation
                fringe-indicator-alist)
          fringe-indicator-alist)                  ; Remove continuation arrow on the right frame
  help-window-select t                             ; Focus new help windows when opened
  indent-tabs-mode nil                             ; Stop using tabs to indent
  idle-update-delay 1                              ; Slow down update ui a bit
  inhibit-default-init t                           ; Default initialization
  inhibit-startup-message t                        ; Do not show anythings unnecessary
  initial-scratch-message nil                      ; Empty the initial *scratch* buffer
  inhibit-startup-echo-area-message user-full-name ; Show User name
  initial-major-mode 'fundamental-mode
  jit-lock-defer-time nil                          ; Font lock optimization
  jit-lock-stealth-nice 0.1
  jit-lock-stealth-time 0.2
  jit-lock-stealth-verbose nil
  mode-line-format nil                             ; Disable mode line format when startup
  mouse-yank-at-point t                            ; Yank at point rather than pointer
  ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
  recenter-positions '(5 top bottom)               ; Set re-centering positions
  ffap-machine-p-known 'reject                     ; Dont ping thing that look like domain
  frame-inhibit-implied-resize t                   ; Frame optimization
  highlight-nonselected-windows nil                ; Remove highlight on nonselected windows
  hscroll-margin 2                                 ; Horizontal scroll
  hscroll-step 1                                   ; Horizontal step
  fast-but-imprecise-scrolling t                   ; Fast scrolling
  recenter-positions '(top middle bottom)          ; Setup recenter
  scroll-conservatively most-positive-fixnum       ; Always scroll by one line
  scroll-margin 0                                  ; Add a margin when scrolling vertically
  scroll-preserve-screen-position t                ; Reduce cursor lag a bit
  mouse-wheel-scroll-amount '(5 ((shift) . 2))     ; Shift Mouse wheel
  mouse-wheel-progressive-speed nil                ; don't accelerate scrolling
  select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
  sentence-end-double-space nil                    ; End a sentence after a dot and a space
  show-help-function nil                           ; Disable help messages
  show-trailing-whitespace nil                     ; Display trailing whitespaces
  split-height-threshold nil                       ; Disable vertical window splitting
  split-width-threshold nil                        ; Disable horizontal window splitting
  tab-width 4                                      ; Set width for tabs
  use-file-dialog nil                              ; Disable use file dialog
  use-dialog-box nil                               ; Disable use dialog box
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
(prefer-coding-system 'utf-8-unix)
(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8)               ; included by set-selection-coding-system
(set-keyboard-coding-system 'utf-8)                ; configured by prefer-coding-system
(set-terminal-coding-system 'utf-8)                ; configured by prefer-coding-system
(setq buffer-file-coding-system 'utf-8)            ; utf-8-unix
(setq save-buffer-coding-system 'utf-8-unix)       ; nil
(setq process-coding-system-alist
      (cons '("grep" utf-8 . utf-8) process-coding-system-alist)
)
(toggle-frame-maximized)                           ; Toggle maximized

;; MacOS tweak
(when sys/macos
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil
        ns-pop-up-frames nil
        initial-frame-alist (append '((ns-transparent-titlebar . t)
                                      (ns-appearance . dark)
                                     )
                            )
  )
  (and (or (daemonp)
           (display-graphic-p))
       (require 'ns-auto-titlebar nil t)
       (ns-auto-titlebar-mode +1)
  )
  (setq mac-command-modifier 'meta) ; make cmd key do Meta
  (setq mac-option-modifier 'super) ; make opt key do Super
  (setq mac-control-modifier 'control) ; make Control key do Control
  (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
)

;; Set up cursor
;; (setq-default cursor-type 'bar)
(setq-default cursor-in-non-selected-windows 'hollow)
(setq x-stretch-cursor nil)
(setq visible-cursor nil)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

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
  "Backup files in a designated FPATH."
  (let* (
        (backupRootDir (concat plocal-dir "_backup"))
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
        )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
  )
)
(setq make-backup-file-name-function 'my_backup_file_name)


;; personal settings will be written to user.el
(setq custom-file (expand-file-name "~/.emacs.d/.local/user.el" user-emacs-directory))

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

;; Remove unnecessary error warnings
(defun penguin-command-error-function (data context caller)
  "Ignore the `buffer-read-only',`beginning-of-buffer',`end-of-buffer' signals.
Then pass DATA, CONTEXT & CALLER to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller))
)

(setq command-error-function #'penguin-command-error-function)

;;; Optimization
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
)
(unless sys/macos (setq command-line-ns-option-alist nil))
(unless sys/linux (setq command-line-x-option-alist nil))

;; Files
(setq-default
  abbrev-file-name             (concat plocal-dir "abbrev.el")
  auto-save-list-file-name     (concat pcache-dir "autosave")
  pcache-directory             (concat pcache-dir "pcache/")
  recentf-save-file            (concat plocal-dir "recentf")
  mc/list-file                 (concat petc-dir "mc-lists.el")
  server-auth-dir              (concat pcache-dir "server/")
  shared-game-score-directory  (concat petc-dir "shared-game-score/")
  tramp-auto-save-directory    (concat pcache-dir "tramp-auto-save/")
  tramp-backup-directory-alist backup-directory-alist
  tramp-persistency-file-name  (concat pcache-dir "tramp-persistency.el")
  url-cache-directory          (concat pcache-dir "url/")
  url-configuration-directory  (concat petc-dir "url/")
)
(provide 'setup_base)
;;; setup_base.el ends here
