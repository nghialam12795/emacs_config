;; setup_org.el
;; -----------------------------------------------------------
;; Author: Nghia Lam
;; Usage: Setting up Org Mode
;; -----------------------------------------------------------


;; Setup `Org'
(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

(provide 'setup_org)
