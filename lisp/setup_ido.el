;;; setup_ido.el --- Setting up Ido searching systems
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; This file is created based on the author's coding habit.
;; This may not be suitable for some people.

;;; Commentary:
;;
;; Setting up Ido searching systems
;; -----------------------------------------------------------

;;; Code:

;; Enable ido mode
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-ignore-directories '(".git"))
(setq ido-ignore-extensions t)


(provide 'setup_ido)
;;; setup_ido.el ends here
