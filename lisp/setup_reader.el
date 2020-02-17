;;; setup_reader.el --- Settings up packages for reading purpose
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;;
;; Setting up packages for reading purpose
;; -----------------------------------------------------------

;;; Code:
(require 'setup_misc)

;; PDF reader
(when (display-graphic-p)
  (use-package pdf-view
    :ensure pdf-tools
    :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
    :defines pdf-annot-activate-created-annotations
    :functions (penguin/pdf-view-set-midnight-colors penguin/pdf-view-set-dark-theme)
    :commands pdf-view-midnight-minor-mode
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :bind (:map pdf-view-mode-map
           ("C-s" . isearch-forward))
    :init (setq pdf-annot-activate-created-annotations t)
    :config
    ;; WORKAROUND: Fix compilation errors on macOS.
    ;; @see https://github.com/politza/pdf-tools/issues/480
    (when sys/macos
      (setenv "PKG_CONFIG_PATH"
              "/usr/local/lib/pkgconfig:/usr/local/opt/libffi/lib/pkgconfig"))
    (pdf-tools-install t nil t t)

    ;; Set dark theme
    (defun penguin/pdf-view-set-midnight-colors ()
      "Set pdf-view midnight colors."
      (setq pdf-view-midnight-colors
            `(,(face-foreground 'default) . ,(face-background 'default))))

    (defun penguin/pdf-view-set-dark-theme ()
      "Set pdf-view midnight theme as color theme."
      (penguin/pdf-view-set-midnight-colors)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (eq major-mode 'pdf-view-mode)
            (pdf-view-midnight-minor-mode (if pdf-view-midnight-minor-mode 1 -1))))))

    (penguin/pdf-view-set-midnight-colors)
    (add-hook 'after-load-theme-hook #'penguin/pdf-view-set-dark-theme)

    ;; FIXME: Support retina
    ;; @see https://emacs-china.org/t/pdf-tools-mac-retina-display/10243/
    ;; and https://github.com/politza/pdf-tools/pull/501/
    (setq pdf-view-use-scaling t
          pdf-view-use-imagemagick nil)
    (with-no-warnings
      (defun pdf-view-use-scaling-p ()
        "Return t if scaling should be used."
        (and (or (and (eq system-type 'darwin) (>= emacs-major-version 27))
                 (memq (pdf-view-image-type) '(imagemagick image-io)))
             pdf-view-use-scaling))
      (defun pdf-view-create-page (page &optional window)
        "Create an image of PAGE for display on WINDOW."
        (let* ((size (pdf-view-desired-image-size page window))
               (width (if (not (pdf-view-use-scaling-p))
                          (car size)
                        (* 2 (car size))))
               (data (pdf-cache-renderpage
                      page width width))
               (hotspots (pdf-view-apply-hotspot-functions
                          window page size)))
          (pdf-view-create-image data
            :width width
            :scale (if (pdf-view-use-scaling-p) 0.5 1)
            :map hotspots
            :pointer 'arrow))))

    ;; Recover last viewed position
    (when emacs/>=26p
      (use-package pdf-view-restore
        :hook (pdf-view-mode . pdf-view-restore-mode)
        :init (setq pdf-view-restore-filename
                    (locate-user-emacs-file ".pdf-view-restore"))
      )
    )
  )
)

;; Epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :functions centaur-read-mode
  :hook (nov-mode . penguin/nov-setup)
  :init
  (defun penguin/nov-setup ()
    "Setup `nov-mode' for better reading experience."
    (visual-line-mode 1)
    (centaur-read-mode)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5))
  :config
  ;; FIXME: errors while opening `nov' files with Unicode characters
  ;; @see https://github.com/wasamasa/nov.el/issues/63
  (with-no-warnings
    (defun penguin/nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (when-let* ((name (nov-content-unique-identifier-name content))
                  (selector (format "package>metadata>identifier[id='%s']"
                                    (regexp-quote name)))
                  (id (car (esxml-node-children (esxml-query selector content)))))
        (intern id)))
    (advice-add #'nov-content-unique-identifier :override #'penguin/nov-content-unique-identifier))

  ;; Fix encoding issue on Windows
  (when sys/win32
    (setq process-coding-system-alist
          (cons `(,nov-unzip-program . (gbk . gbk))
                process-coding-system-alist)
    )
  )
)




;; A stackoverflow and its sisters' sites reader
(when emacs/>=26p
  (use-package howdoyou
    :bind (:map howdoyou-mode-map
           ("q" . kill-buffer-and-window))
    :hook (howdoyou-mode . read-only-mode))
)



(provide 'setup_reader)
;;; setup_reader.el ends here
