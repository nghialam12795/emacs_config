;;; setup_json.el --- Setting packages for supporting Json
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Setting packages for supporting Json
;; -----------------------------------------------------------

;;; Code:
(use-package json-mode
  :delight "J "
  :mode "\\.json\\'"
  :hook (before-save . penguin/json-mode-before-save-hook)
  :preface
  (defun penguin/json-mode-before-save-hook ()
    (when (eq major-mode 'json-mode)
      (json-pretty-print-buffer)))

  (defun penguin/json-array-of-numbers-on-one-line (encode array)
    "Prints the arrays of numbers in one line."
    (let* ((json-encoding-pretty-print
            (and json-encoding-pretty-print
                 (not (loop for x across array always (numberp x)))))
           (json-encoding-separator (if json-encoding-pretty-print "," ", ")))
      (funcall encode array)))
  :config (advice-add 'json-encode-array :around #'penguin/json-array-of-numbers-on-one-line)
)

(provide 'setup_json)
;;; setup_json.el ends here
