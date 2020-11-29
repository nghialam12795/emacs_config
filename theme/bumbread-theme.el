;;; bumbread-theme.el --- bumbread
;;; Version: 1.0
;;; Commentary:
;;; A theme called bumbread
;;; Code:

(deftheme bumbread "My custom theme based on bumbread")
  (custom-theme-set-faces 'bumbread
   '(default ((t (:foreground "#bcbcbc" :background "#090d12" ))))
   '(cursor ((t (:background "white" ))))
   '(fringe ((t (:background "#090d12" ))))
   '(mode-line ((t (:foreground "#363636" :background "#ffffff" ))))
   '(region ((t (:background "#434343" ))))
   '(secondary-selection ((t (:background "#343434" ))))
   '(font-lock-builtin-face ((t (:foreground "#ffffff" ))))
   '(font-lock-comment-face ((t (:foreground "#555555" ))))
   '(font-lock-function-name-face ((t (:foreground "#ffffff" ))))
   '(font-lock-keyword-face ((t (:foreground "#ffffff" ))))
   '(font-lock-string-face ((t (:foreground "#a8a59e" ))))
   '(font-lock-type-face ((t (:foreground "#ffffff" ))))
   '(font-lock-constant-face ((t (:foreground "#ffffff" ))))
   '(font-lock-variable-name-face ((t (:foreground "#bcbcbc" ))))
   '(minibuffer-prompt ((t (:foreground "#bcbcbc" :bold t ))))
   '(font-lock-warning-face ((t (:foreground "red" :bold t )))))

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'bumbread)

;;; bumbread-theme.el ends here
