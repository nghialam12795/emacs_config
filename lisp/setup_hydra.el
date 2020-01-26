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
  :preface
  (defvar-local penguin/ongoing-hydra-body nil)
  (defun penguin/ongoing-hydra ()
    (interactive)
    (if penguin/ongoing-hydra-body
        (funcall penguin/ongoing-hydra-body)
      (user-error "Error: penguin/ongoing-hydra-body is not set")
    )
  )
  :bind
  ("C-c a" . hydra-drawibm/body)
  ("C-c f" . hydra-flycheck/body)
  ("C-c g" . hydra-magit/body)
  ("C-c i" . hydra-ivy/body)
  ("C-c o" . hydra-org/body)
  ("C-c h" . hydra-projectile/body)
  ("C-c s" . hydra-system/body)
  ("C-c w" . hydra-windows/body)
  :custom
  (hydra-default-hint nil)
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


;; ------------------------------------
;; `flycheck'
;; ------------------------------------
(defhydra hydra-flycheck (:color blue)
  "
^
^Flycheck^          ^Errors^            ^Checker^
^────────^──────────^──────^────────────^───────^───────────
_q_ quit            _<_ previous        _?_ describe
_m_ manual          _>_ next            _d_ disable
_v_ verify setup    _f_ check           _s_ select
^^                  _l_ list            ^^
^^                  ^^                  ^^
"
  ("q" nil)
  ("<" flycheck-previous-error :color pink)
  (">" flycheck-next-error :color pink)
  ("?" flycheck-describe-checker)
  ("d" flycheck-disable-checker)
  ("f" flycheck-buffer)
  ("l" flycheck-list-errors)
  ("m" flycheck-manual)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup)
)

;; ------------------------------------
;; `ivy'
;; ------------------------------------
(defhydra hydra-ivy (:color pink)
  "
^
^Ivy^               ^Do^                ^Browse^
^────────^──────────^──^────────────────^──────^────────────
_q_ quit            _m_ mark            _l_ libraries
_r_ resume          _M_ unmark          _s_ symbols
^^                  ^^                  _u_ unicode
^^                  ^^                  ^^
"
  ("q" nil)
  ("l" counsel-find-library :color blue)
  ("m" ivy-mark)
  ("M" ivy-unmark)
  ("r" ivy-resume :color blue)
  ("s" counsel-info-lookup-symbol :color blue)
  ("u" counsel-unicode-char :color blue)
)

;; ------------------------------------
;; `magit'
;; ------------------------------------
(defhydra hydra-magit (:color blue)
  "
^
^Magit^             ^Do^
^─────^─────────────^──^────────────────
_q_ quit            _b_ blame
^^                  _c_ clone
^^                  _i_ init
^^                  _s_ status
^^                  ^^
"
  ("q" nil)
  ("b" magit-blame)
  ("c" magit-clone)
  ("i" magit-init)
  ("s" magit-status)
)

;; ------------------------------------
;; `markdown'
;; ------------------------------------
(defhydra hydra-markdown (:color pink)
  "
^
^Markdown^          ^Table Columns^     ^Table Rows^
^────────^──────────^─────────────^─────^──────────^────────
_q_ quit            _c_ insert          _r_ insert
^^                  _C_ delete          _R_ delete
^^                  _M-<left>_ left     _M-<down>_ down
^^                  _M-<right>_ right   _M-<up>_ up
^^                  ^^                  ^^
"
  ("q" nil)
  ("c" markdown-table-insert-column)
  ("C" markdown-table-delete-column)
  ("r" markdown-table-insert-row)
  ("R" markdown-table-delete-row)
  ("M-<left>" markdown-table-move-column-left)
  ("M-<right>" markdown-table-move-column-right)
  ("M-<down>" markdown-table-move-row-down)
  ("M-<up>" markdown-table-move-row-up)
)

;; ------------------------------------
;; `org'
;; ------------------------------------
(defhydra hydra-org (:color pink)
  "
^
^Org^               ^Links^             ^Outline^
^───^───────────────^─────^─────────────^───────^───────────
_q_ quit            _i_ insert          _<_ previous
^^                  _n_ next            _>_ next
^^                  _p_ previous        _a_ all
^^                  _s_ store           _g_ go
^^                  ^^                  _v_ overview
^^                  ^^                  ^^
"
  ("q" nil)
  ("<" org-backward-element)
  (">" org-forward-element)
  ("a" outline-show-all)
  ("g" counsel-org-goto :color blue)
  ("i" org-insert-link :color blue)
  ("n" org-next-link)
  ("p" org-previous-link)
  ("s" org-store-link)
  ("v" org-overview)
)

;; ------------------------------------
;; `projectile'
;; ------------------------------------
(defhydra hydra-projectile (:color blue)
  "
^
^Projectile^        ^Buffers^           ^Find^              ^Search^
^──────────^────────^───────^───────────^────^──────────────^──────^────────────
_q_ quit            [_b_] list          [_d_] directory     [_s_] swiper
_i_ reset cache     [_k_] kill buffer   [_D_] root          [_S_] helm-grep
^^                  [_K_] kill all      [_f_] file          ^^
^^                  ^^                  [_p_] project       ^^
^^                  ^^                  ^^                  ^^
"
  ("q" nil)
  ("b" counsel-projectile-switch-to-buffer)
  ("d" counsel-projectile-find-dir)
  ("D" projectile-dired)
  ("f" counsel-projectile-find-file)
  ("i" projectile-invalidate-cache :color red)
  ("k" kill-this-buffer)
  ("K" projectile-kill-buffers)
  ("p" counsel-projectile-switch-project)
  ("s" swiper)
  ("S" helm-projectile-grep)
)

;; ------------------------------------
;; `hydra'
;; ------------------------------------
(defhydra hydra-system (:color blue)
  "
^
^System^            ^Packages^          ^Processes^         ^Shell^
^──────^────────────^────────^──────────^─────────^─────────^─────^─────────────
_q_ quit            _p_ list            _s_ list            _e_ eshell
^^                  _P_ upgrade         ^^                  _t_ term
^^                  ^^                  ^^                  _T_ ansi-term
"
  ("q" nil)
  ("e" (eshell t))
  ("p" paradox-list-packages)
  ("P" paradox-upgrade-packages)
  ("s" list-processes)
  ("t" term)
  ("T" ansi-term)
)

;; ------------------------------------
;; `window'
;; ------------------------------------
(defhydra hydra-windows (:color pink)
  "
                                                              ^^^^^^^╭─────────╮^
 ^Resize^          ^Select^           ^Split^                       ^│ Windows │^
─^──────^──────────^──────^───────────^─────^───────────────────────^┴─────────╯^
     ^_i_^               ^_p_^            [_v_] Split Vertically
     ^^↑^^               ^^↑^^            [_x_] Split Horizontally
 _j_ ←   → _l_       _b_ ←   → _f_        [_d_] Delete other windows
     ^^↓^^               ^^↓^^            ^^
     ^_k_^               ^_n_^            ^^
 _g_ balance             ^^               ^^
────────────────────
_q_ to quit.
"
  ("q" nil)
  ("g" balance-windows)
  ("i" enlarge-window)
  ("j" shrink-window-horizontally)
  ("k" shrink-window)
  ("l" enlarge-window-horizontally)
  ("b" windmove-left nil)
  ("f" windmove-right nil)
  ("p" windmove-up nil)
  ("n" windmove-down nil)
  ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right)
       )
  )
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)
       )
  )
  ("d" delete-other-windows :color blue)
)

(provide 'setup_hydra)
;;; setup_hydra.el ends here
