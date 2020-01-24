;;; setup_keyboard.el --- Setting default keyboard for my usage
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based o the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Setting default keyboard for my usage
;; -----------------------------------------------------------

;;; Code:


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
  "Quickly duplicate the current line down."
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

;; Smarter `C-a'
(global-set-key [remap move-beginning-of-line] #'penguin/beginning-of-line-dwim)
(defun penguin/beginning-of-line-dwim ()
  "Move point to first non-whitespace character, or beginning of line."
  (interactive "^")
  (let ((origin (point)))
    (beginning-of-line)
    (and (= origin (point))
         (back-to-indentation)
    )
  )
)

;; `Move text'
(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down))
  :config (move-text-default-bindings)
)

;; Always quit minibuffer
(use-package delsel
  :bind
  (:map mode-specific-map
        ("C-g" . minibuffer-keyboard-quit)
  )
)

;; Transpose frame
(use-package transpose-frame
  :ensure t
  :defer t
  :bind   ("C-x |" . transpose-frame)
)

;; Smart comment
(use-package smart-comment
  :bind ("M-;" . smart-comment)
)

(defun penguin/switch-to-buffer-continue ()
  "Activate a sparse keymap:
<left>   `previous-buffer'
<right>  `next-buffer'"
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<left>") #'previous-buffer)
     (define-key map (kbd "<right>") #'next-buffer)
     map
   )
  )
)
(advice-add 'previous-buffer :after #'penguin/switch-to-buffer-continue)
(advice-add 'next-buffer :after #'penguin/switch-to-buffer-continue)

;; Custom setup
(define-key global-map (kbd "C-G") 'ff-find-other-file)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key [remap kill-buffer] #'kill-this-buffer)
(global-unset-key (kbd "C-z")) ;; Remove annoying keymap

(provide 'setup_keyboard)
;;; setup_keyboard.el ends here
