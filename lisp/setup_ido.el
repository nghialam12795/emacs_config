;; setup_ido.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up the ido searching systems
;; -----------------------------------------------------------

;; Enable ido mode
(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-ignore-directories '(".git"))
(setq ido-ignore-extensions t)


(provide 'setup_ido)
