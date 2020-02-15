;;; setup_python.el --- Setting up Packages for python development
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Setting up packages for Python development
;; -----------------------------------------------------------

;;; Code:
(require 'setup_misc)

(defvar python-pyenv-root nil
  "The path to pyenv's root directory. This is automatically set when `python' is loaded."
)
(defvar python-pyenv-versions nil
  "Available versions of python in pyenv."
)
(defvar-local python-current-version nil
  "The current active pyenv version."
)

(use-package python
  :commands python-mode
  :init
  (setq python-enviroment-directory pcache-dir
        python-indent-gues-indent-offset-verbose nil
        python-shell-interpreter "python"
  )
  :config
  (add-hook 'python-mode-hook #'flycheck-mode)
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt --no-color-info"
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-setup-code
          "from IPython.core.completerlib import module_completion"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
    )
  )

  ;; Version Management
  (defun penguin/python-add-ver-to-modeline ()
    "Add version string to the major mode in the modeline."
    (setq mode-name
          (if python-current-version
              (format "Python %s" python-current-version)
            "Python")
    )
  )
  (add-hook 'python-mode-hook #'penguin/add-pythonver-to-modeline)

  (if (not (executable-find "pyenv"))
      (setq python-current-version (string-trim (shell-command-to-string "python --version 2>&1 | cut -d' ' -f2")))
    (setq python-pyenv-root     (string-trim (shell-command-to-string "pyenv root"))
          python-pyenv-versions (split-string (shell-command-to-string "pyenv versions --bare") "\n" t)
    )
    
    (defun penguin/python-detect-pyenv-version ()
      "Detect the pyenv version for the current project and set the relevant environment variables."
      (when-let* ((version-str (shell-command-to-string "python --version 2>&1 | cut -d' ' -f2")))
        (setq version-str (string-trim version-str)
              python-current-version version-str)
        (let ((pyenv-current-path (concat python-pyenv-root "/versions/" version-str)))
          (when (file-directory-p pyenv-current-path)
            (setq pythonic-environment pyenv-current-path)))
        (when (member version-str python-pyenv-versions)
          (setenv "PYENV_VERSION" version-str))
      )
    )
    (add-hook 'python-mode-hook #'penguin/python-detect-pyenv-version)
  )
)

(use-package anaconda-mode
  :after python
  :hook python-mode
  :init
  (setq anaconda-mode-installation-directory (concat petc-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t
  )
  :config
  (add-hook 'anaconda-mode-hook #'anaconda-eldoc-mode)
)

(use-package company-anaconda
  :after anaconda-mode
)

(use-package pip-requirements
  :mode ("requirements.txt$" . pip-requirements-mode)
)

(use-package nose
  :commands nose-mode
  :preface
  (defvar nose-mode-map (make-sparse-keymap))
)



(provide 'setup_python)
;;; setup_python.el ends here
