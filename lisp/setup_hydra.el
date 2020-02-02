;;; setup_hydra.el --- Setting up the Hydra packages for better navigation
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;;
;; Setting up the Hydra packages for better navigation
;; -----------------------------------------------------------

;;; Code:

;; ------------------------------------
;; Prerequisite `Hydra'
;; ------------------------------------
(use-package hydra
  :config
  (use-package hydra-posframe
    :load-path "~/.emacs.d/site_lisp/"
    :custom
    (hydra-posframe-parameters
      '((left-fringe . 5)
        (right-fringe . 5))
    )
    :custom-face
    (hydra-posframe-border-face ((t (:background "#6272a4"))))
    :hook (after-init . hydra-posframe-enable)
  )
  :bind
  ("C-c h a" . hydra-drawibm/body)
  :custom
  (hydra-default-hint nil)
)
(use-package pretty-hydra
  :bind
  ("C-c h l" . hydra-lspui/body)
  ("C-c h f" . hydra-flycheck/body)
  ("C-c h w" . hydra-window/body)
)

;; Misc functions
(defun phydra-title (title &optional icon-type icon-name
                           &key face height v-adjust)
      "Add an icon in the hydra title."
      (let ((face (or face `(:foreground ,(face-background 'highlight))))
            (height (or height 1.0))
            (v-adjust (or v-adjust 0.0))
           )
        (concat
         (when (and (display-graphic-p) icon-type icon-name)
           (let ((f (intern (format "all-the-icons-%s" icon-type))))
             (when (fboundp f)
               (concat
                (apply f (list icon-name :face face :height height :v-adjust v-adjust))
                " "))))
         (propertize title 'face face)
        )
      )
)

;; Custom settings
;; ------------------------------------
;; `ibm-drawing'
;; ------------------------------------
(defcustom penguin/ibm-overwrite nil "Overwrite mode for IBM (codepage 437) box drawing.")
(defun penguin/ibm-insert (char)
  "Insert CHAR with conditional overwrite."
  (interactive)
  (when penguin/ibm-overwrite
    (kill-char 1))
  (insert char)
)

(defhydra hydra-drawibm (:color pink)
  "
IBM Box Chars  _r_ ─         _R_ ═         _v_ │         _V_ ║
(CodePage 437) _q_ ┌ _w_ ┬ _e_ ┐ _Q_ ╒ _W_ ╤ _E_ ╕ _t_ ╔ _y_ ╦ _u_ ╗ _T_ ╓ _Y_ ╥ _U_ ╖  _C-q_ ╭ ╮ _C-w_
               _a_ ├ _s_ ┼ _d_ ┤ _A_ ╞ _S_ ╪ _D_ ╡ _g_ ╠ _h_ ╬ _j_ ╣ _G_ ╟ _H_ ╫ _J_ ╢
               _z_ └ _x_ ┴ _c_ ┘ _Z_ ╘ _X_ ╧ _C_ ╛ _b_ ╚ _n_ ╩ _m_ ╝ _B_ ╙ _N_ ╨ _M_ ╜  _C-a_ ╰ ╯ _C-s_
_ESC_ to exit    _i_ Toggle Overwrite/Insert
"
   ("ESC" nil nil :color blue)
   ("<space>"   (search-backward "+"))
   ("S-<space>" (search-forward "+"))
   ("q" (penguin/ibm-insert "┌")) ("w" (penguin/ibm-insert "┬")) ("e" (penguin/ibm-insert "┐"))
   ("Q" (penguin/ibm-insert "╒")) ("W" (penguin/ibm-insert "╤")) ("E" (penguin/ibm-insert "╕"))
   ("t" (penguin/ibm-insert "╔")) ("y" (penguin/ibm-insert "╦")) ("u" (penguin/ibm-insert "╗"))
   ("T" (penguin/ibm-insert "╓")) ("Y" (penguin/ibm-insert "╥")) ("U" (penguin/ibm-insert "╖"))
   ("a" (penguin/ibm-insert "├")) ("s" (penguin/ibm-insert "┼")) ("d" (penguin/ibm-insert "┤"))
   ("A" (penguin/ibm-insert "╞")) ("S" (penguin/ibm-insert "╪")) ("D" (penguin/ibm-insert "╡"))
   ("g" (penguin/ibm-insert "╠")) ("h" (penguin/ibm-insert "╬")) ("j" (penguin/ibm-insert "╣"))
   ("G" (penguin/ibm-insert "╟")) ("H" (penguin/ibm-insert "╫")) ("J" (penguin/ibm-insert "╢"))
   ("z" (penguin/ibm-insert "└")) ("x" (penguin/ibm-insert "┴")) ("c" (penguin/ibm-insert "┘"))
   ("Z" (penguin/ibm-insert "╘")) ("X" (penguin/ibm-insert "╧")) ("C" (penguin/ibm-insert "╛"))
   ("b" (penguin/ibm-insert "╚")) ("n" (penguin/ibm-insert "╩")) ("m" (penguin/ibm-insert "╝"))
   ("B" (penguin/ibm-insert "╙")) ("N" (penguin/ibm-insert "╨")) ("M" (penguin/ibm-insert "╜"))
   ("r" (penguin/ibm-insert "─")) ("R" (penguin/ibm-insert "═"))
   ("v" (penguin/ibm-insert "│")) ("V" (penguin/ibm-insert "║"))
   ("C-q" (penguin/ibm-insert "╭")) ("C-w" (penguin/ibm-insert "╮"))
   ("C-a" (penguin/ibm-insert "╰")) ("C-s" (penguin/ibm-insert "╯"))
   ("i" (setq penguin/ibm-overwrite (not penguin/ibm-overwrite)) )
)


;; ;; ------------------------------------
;; ;; `flycheck'
;; ;; ------------------------------------
(pretty-hydra-define hydra-flycheck (:title (phydra-title "Flycheck" 'faicon "bug")
                                     :color pink
                                     :quit-key "q"
                                    )
  ("Documentation"
   (("m" flycheck-manual "manual" :exit t)
    ("v" flycheck-verify-setup "verify setup" :exit t)
   )
   "Errors"
   ((">" flycheck-next-error "next")
    ("<" flycheck-previous-error "previous")
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list")
   )
   "Checker"
   (("?" flycheck-describe-checker "describe")
    ("d" flycheck-disable-checker "disable")
    ("s" flycheck-select-checker "select")
   )
  )
)


;; ;; ------------------------------------
;; ;; `ivy'
;; ;; ------------------------------------
;; (defhydra hydra-ivy (:color pink)
;;   "
;; ^
;; ^Ivy^               ^Do^                ^Browse^
;; ^────────^──────────^──^────────────────^──────^────────────
;; _q_ quit            _m_ mark            _l_ libraries
;; _r_ resume          _M_ unmark          _s_ symbols
;; ^^                  ^^                  _u_ unicode
;; ^^                  ^^                  ^^
;; "
;;   ("q" nil)
;;   ("l" counsel-find-library :color blue)
;;   ("m" ivy-mark)
;;   ("M" ivy-unmark)
;;   ("r" ivy-resume :color blue)
;;   ("s" counsel-info-lookup-symbol :color blue)
;;   ("u" counsel-unicode-char :color blue)
;; )

;; ;; ------------------------------------
;; ;; `magit'
;; ;; ------------------------------------
;; (defhydra hydra-magit (:color blue)
;;   "
;; ^
;; ^Magit^             ^Do^
;; ^─────^─────────────^──^────────────────
;; _q_ quit            _b_ blame
;; ^^                  _c_ clone
;; ^^                  _i_ init
;; ^^                  _s_ status
;; ^^                  ^^
;; "
;;   ("q" nil)
;;   ("b" magit-blame)
;;   ("c" magit-clone)
;;   ("i" magit-init)
;;   ("s" magit-status)
;; )

;; ;; ------------------------------------
;; ;; `markdown'
;; ;; ------------------------------------
;; (defhydra hydra-markdown (:color pink)
;;   "
;; ^
;; ^Markdown^          ^Table Columns^     ^Table Rows^
;; ^────────^──────────^─────────────^─────^──────────^────────
;; _q_ quit            _c_ insert          _r_ insert
;; ^^                  _C_ delete          _R_ delete
;; ^^                  _M-<left>_ left     _M-<down>_ down
;; ^^                  _M-<right>_ right   _M-<up>_ up
;; ^^                  ^^                  ^^
;; "
;;   ("q" nil)
;;   ("c" markdown-table-insert-column)
;;   ("C" markdown-table-delete-column)
;;   ("r" markdown-table-insert-row)
;;   ("R" markdown-table-delete-row)
;;   ("M-<left>" markdown-table-move-column-left)
;;   ("M-<right>" markdown-table-move-column-right)
;;   ("M-<down>" markdown-table-move-row-down)
;;   ("M-<up>" markdown-table-move-row-up)
;; )

;; ;; ------------------------------------
;; ;; `org'
;; ;; ------------------------------------
;; (defhydra hydra-org (:color pink)
;;   "
;; ^
;; ^Org^               ^Links^             ^Outline^
;; ^───^───────────────^─────^─────────────^───────^───────────
;; _q_ quit            _i_ insert          _<_ previous
;; ^^                  _n_ next            _>_ next
;; ^^                  _p_ previous        _a_ all
;; ^^                  _s_ store           _g_ go
;; ^^                  ^^                  _v_ overview
;; ^^                  ^^                  ^^
;; "
;;   ("q" nil)
;;   ("<" org-backward-element)
;;   (">" org-forward-element)
;;   ("a" outline-show-all)
;;   ("g" counsel-org-goto :color blue)
;;   ("i" org-insert-link :color blue)
;;   ("n" org-next-link)
;;   ("p" org-previous-link)
;;   ("s" org-store-link)
;;   ("v" org-overview)
;; )

;; ;; ------------------------------------
;; ;; `projectile'
;; ;; ------------------------------------
;; (defhydra hydra-projectile (:color blue)
;;   "
;; ^
;; ^Projectile^        ^Buffers^           ^Find^              ^Search^
;; ^──────────^────────^───────^───────────^────^──────────────^──────^────────────
;; _q_ quit            [_b_] list          [_d_] directory     [_s_] swiper
;; _i_ reset cache     [_k_] kill buffer   [_D_] root          [_S_] helm-grep
;; ^^                  [_K_] kill all      [_f_] file          ^^
;; ^^                  ^^                  [_p_] project       ^^
;; ^^                  ^^                  ^^                  ^^
;; "
;;   ("q" nil)
;;   ("b" counsel-projectile-switch-to-buffer)
;;   ("d" counsel-projectile-find-dir)
;;   ("D" projectile-dired)
;;   ("f" counsel-projectile-find-file)
;;   ("i" projectile-invalidate-cache :color red)
;;   ("k" kill-this-buffer)
;;   ("K" projectile-kill-buffers)
;;   ("p" counsel-projectile-switch-project)
;;   ("s" swiper)
;;   ("S" helm-projectile-grep)
;; )

;; ;; ------------------------------------
;; ;; `hydra'
;; ;; ------------------------------------
;; (defhydra hydra-system (:color blue)
;;   "
;; ^
;; ^System^            ^Packages^          ^Processes^         ^Shell^
;; ^──────^────────────^────────^──────────^─────────^─────────^─────^─────────────
;; _q_ quit            _p_ list            _s_ list            _e_ eshell
;; ^^                  _P_ upgrade         ^^                  _t_ term
;; ^^                  ^^                  ^^                  _T_ ansi-term
;; "
;;   ("q" nil)
;;   ("e" (eshell t))
;;   ("p" paradox-list-packages)
;;   ("P" paradox-upgrade-packages)
;;   ("s" list-processes)
;;   ("t" term)
;;   ("T" ansi-term)
;; )

;; ------------------------------------
;; `window'
;; ------------------------------------
(use-package ace-window)
(pretty-hydra-define hydra-window (:foreign-keys warn
                                   :title (phydra-title "Windows Management" 'faicon "windows")
                                   :quit-key "q"
                                   :color amaranth
                                  )
  ("Actions"
   (("TAB" other-window "switch")
    ("x" ace-delete-window "delete")
    ("m" ace-delete-other-windows "maximize")
    ("s" ace-swap-window "swap")
    ("a" ace-select-window "select")
   )

   "Resize"
   (("i" enlarge-window "↑ up")
    ("k" shrink-window "↓ down")
    ("j" shrink-window-horizontally "← left")
    ("l" enlarge-window-horizontally "→ right")
    ("n" balance-windows "balance")
    ("f" toggle-frame-fullscreen "toggle fullscreen")
   )

   "Select"
   (("p" windmove-up "↑ up")
    ("n" windmove-down "↓ down")
    ("b" windmove-left "← left")
    ("f" windmove-right "→ right")
   )

   "Split"
   (("h" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down)
         ) "horizontally"
    )
    ("v" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right)
         ) "vertically"
    )
   )
  )
)

;; ------------------------------------
;; `dashboard'
;; ------------------------------------
(pretty-hydra-define hydra-dashboard (:title (phydra-title "Dashboard" 'material "dashboard")
                                      :color pink
                                      :quit-key "q"
                                     )
  ("Navigator"
   (("U" update-config-and-packages "update" :exit t)
    ("H" browse-homepage "homepage" :exit t)
    ("R" restore-session "recover session" :exit t)
    ("L" persp-load-state-from-file "list sessions" :exit t)
    ("S" open-custom-file "settings" :exit t)
   )
   "Section"
   (("}" dashboard-next-section "next")
    ("{" dashboard-previous-section "previous")
    ("r" dashboard-goto-recent-files "recent files")
    ("m" dashboard-goto-bookmarks "projects")
    ("p" dashboard-goto-projects "bookmarks")
   )
   "Item"
   (("RET" widget-button-press "open" :exit t)
    ("<tab>" widget-forward "next")
    ("C-i" widget-forward "next")
    ("<backtab>" widget-backward "previous")
    ("C-n" next-line "next line")
    ("C-p" previous-line "previous  line")
   )
   "Misc"
   (("<f2>" open-dashboard "open" :exit t)
    ("g" dashboard-refresh-buffer "refresh" :exit t)
    ("Q" quit-dashboard "quit" :exit t)
   )
  )
)

;; ------------------------------------
;; `lsp-ui'
;; ------------------------------------
(defun penguin/toggle-lsp-ui-doc ()
  "Toggle the LSP UI."
  (interactive)
  (if lsp-ui-doc-mode
    (progn
      (lsp-ui-doc-mode -1)
      (lsp-ui-doc--hide-frame))
    (lsp-ui-doc-mode 1)
  )
)
(pretty-hydra-define hydra-lspui (:title (phydra-title "LSP UI" 'faicon "rocket")
                                  :color amaranth
                                  :quit-key "q"
                                 )
   ("Doc"
    (("d e" penguin/toggle-lsp-ui-doc "enable" :toggle t)
     ("d s" lsp-ui-doc-include-signature "signature" :toggle t)
     ("d t" (setq lsp-ui-doc-position 'top) "top" :toggle (eq lsp-ui-doc-position 'top))
     ("d b" (setq lsp-ui-doc-position 'bottom) "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
     ("d p" (setq lsp-ui-doc-position 'at-point) "at point" :toggle (eq lsp-ui-doc-position 'at-point))
     ;; ("d f" (setq lsp-ui-doc-alignment 'frame) "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
     ;; ("d w" (setq lsp-ui-doc-alignment 'window) "align window" :toggle (eq lsp-ui-doc-alignment 'window))
    )
    "Sideline"
    (("s e" lsp-ui-sideline-enable "enable" :toggle t)
     ("s h" lsp-ui-sideline-show-hover "hover" :toggle t)
     ("s d" lsp-ui-sideline-show-diagnostics "diagnostics" :toggle t)
     ("s s" lsp-ui-sideline-show-symbol "symbol" :toggle t)
     ("s c" lsp-ui-sideline-show-code-actions "code actions" :toggle t)
     ("s i" lsp-ui-sideline-ignore-duplicate "ignore duplicate" :toggle t)
    )
   )
)

(provide 'setup_hydra)
;;; setup_hydra.el ends here
