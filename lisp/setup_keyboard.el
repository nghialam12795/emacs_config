;; setup_keyboard.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up Default Keyboard for my usage
;; -----------------------------------------------------------


;; Setup Key Helper
(use-package which-key
  :diminish
  :config
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.4)
)
(which-key-mode 1)


;; Setup `window_cursor_move'
(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)
      )
    )
  )
)
(global-set-key (kbd "C-c C-b")  'windmove-left)
(global-set-key (kbd "C-c C-f") 'windmove-right)
(global-set-key (kbd "C-c C-p")    'windmove-up)
(global-set-key (kbd "C-c C-n")  'windmove-down)


;; Setup `muliple_cursor'
(use-package multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Setup `quick_dup_line'
(defun quick-dup-line ()
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2))
       )
    (if (eq last-command 'quick-copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))
    )
  )
  (beginning-of-line 2)
  (yank) ;; Can be duplicated more with `C-y'
)
(global-set-key (kbd "C-c C-d") 'quick-dup-line)


;; Setup `ace-jump'
(require 'ace-jump-mode)
(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)



(provide 'setup_keyboard)
