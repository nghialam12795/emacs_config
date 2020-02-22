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

;; Show keymap using on the modeline
(use-package keycast
  :ensure t
  :config
  (setq keycast-window-predicate 'keycast-active-frame-bottom-right-p)
  (setq keycast-separator-width 3)
  (setq keycast-insert-after 'mode-line-buffer-identification)
  (setq keycast-remove-tail-elements t)
)

;; Setup `window_cursor_move'
(defun ignore-error-wrapper (fn)
  "Funtion return new function FN that ignore errors.
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

;; ;; Transpose frame
;; (use-package transpose-frame
;;   :ensure t
;;   :defer t
;;   :bind   ("C-x |" . transpose-frame)
;; )

;; Smart comment
(use-package smart-comment
  :bind ("M-;" . smart-comment)
)

;; Restart Emacs
(use-package restart-emacs
  :ensure t
  :bind* (("C-x M-c" . restart-emacs))
)

;; Make scrolling right
(defun push-mark-no-activate ()
  "Pushes `point` to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
) ; removed the message, visible-mark takes care of this
(defun penguin/scroll-down-with-mark ()
  "Like `scroll-down-command`, but push a mark if this is not a repeat invocation."
  (interactive)
  (unless (equal last-command 'penguin/scroll-down-with-mark)
    (push-mark-no-activate))
  (scroll-down-command)
)
(defun penguin/scroll-up-with-mark ()
  "Like `scroll-up-command`, but push a mark if this is not a repeat invocation."
  (interactive)
  (unless (equal last-command 'penguin/scroll-up-with-mark)
    (push-mark-no-activate))
  (scroll-up-command)
)

(global-set-key (kbd "C-v") 'penguin/scroll-up-with-mark)
(global-set-key (kbd "M-v") 'penguin/scroll-down-with-mark)

;; Custom setup
(define-key global-map (kbd "C-G") 'ff-find-other-file)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key [remap kill-buffer] #'kill-this-buffer)
(global-set-key (kbd "C-x 3") (lambda () (interactive)(split-window-right) (other-window 1)))
(global-set-key (kbd "C-x 2") (lambda () (interactive)(split-window-below) (other-window 1)))
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-frame)
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key (kbd "M-P") 'previous-buffer)
(global-unset-key (kbd "C-z")) ;; Remove annoying keymap
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-h h"))

(provide 'setup_keyboard)
;;; setup_keyboard.el ends here
