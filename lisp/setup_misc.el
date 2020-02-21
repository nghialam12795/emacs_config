;;; setup_misc.el --- Setting up the constants, ultilities function for further setup
;; -----------------------------------------------------------
;; Author: Nghia Lam
;;
;; Please be aware that this file is created based on the author coding perspective,
;; this may not be suitable for some people.

;;; Commentary:
;; 
;; Setting up the constants, ultilities function for further setup
;; -----------------------------------------------------------

;;; Code:

(defconst pemacs/version
  "0.1.1"
  "Penguin Emacs version."
)

(defconst my-homepage
  "https://github.com/nghialam12795"
  "My personal Github page."
)

(defconst sys/win32
  (eq system-type 'windows-nt)
  "Are we using a Window OS?"
)

(defconst sys/linux
  (eq system-type 'gnu/linux)
  "Are we using a GNU/Linux OS?"
)

(defconst sys/macos
  (eq system-type 'darwin)
  "Are we using a Mac OS?"
)

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above."
)

(defvar pemacs-dir
  (file-truename user-emacs-directory)
  "The path to this emacs.d directory."
)

(defvar pcore-dir
  (concat pemacs-dir "core/")
  "Where essential files are stored."
)

(defvar pmodules-dir
  (concat pemacs-dir "modules/")
  "Where configuration modules are stored."
)

(defvar plocal-dir
  (concat pemacs-dir ".local/")
  "Root directory for local Emacs files.
Use this as permanent storage for files that are safe to share
across systems (if this config is symlinked across several computers)."
)

(defvar petc-dir
  (concat plocal-dir "etc/")
  "Directory for non-volatile storage.
Use this for files that don't change much, like servers binaries, external
dependencies or long-term shared data."
)

(defvar pcache-dir
  (concat plocal-dir "cache/")
  "Directory for volatile storage.
Use this for files that change often, like cache files."
)

;; ----------------------------------
(defconst qutebrowser/win32
  "c:/Program Files/qutebrowser/qutebrowser.exe"
  "The installation path of qutebrowser on Window."
)

(defconst qutebrowser/macos
  "/Applications/qutebrowser.app/Contents/MacOS/qutebrowser"
  "The installation path of qutebrowser on MacOS."
)

;; ----------------------------------
;; `FUNCTIONS'
;; ----------------------------------

(defun penguin-emacs-reload-init-file ()
  "Reload your init.el file without restarting Emacs."
  (interactive)
  (load-file "~/.emacs.d/init.el")
)

(defun penguin-emacs-update-config ()
  "Update Penguin Emacs configuration to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (progn
      (message "Updating Penguin Emacs configuration...")
      (cd dir)
      (shell-command "git pull")
      (message "Load new Penguin Emacs configuration...")
      (penguin-emacs-reload-init-file)
      (message "Update finished.")))
)

;; Helper for org-mode
(defun penguin/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task))
      (while (and (not parent-task)
                  (org-up-heading-safe))
        (when (penguin/is-project-p)
          (setq parent-task (point))))
      parent-task))
)
(defun penguin/is-task-stuck ()
  "Return non-nil if the task at point is stuck."
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (this-headline (save-excursion (org-back-to-heading 'invisible-ok) (point))))
       (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
              (has-next))
          (save-excursion
            (forward-line 1)
            (while (and (not has-next)
                        (< (point) subtree-end)
                        (re-search-forward "^\\*+ NEXT\\|DELEGATED " subtree-end t))
              ;; Only skip if there is a deadline for delegated tasks
              (unless (and (member (org-get-todo-state) (list "DELEGATED"))
                           (not (org-element-property :deadline (org-element-at-point))))
                (setq has-next t))))
          (not has-next))))
)
(defun penguin/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (and (nth 2 (org-heading-components))
                          (not (member (nth 2 (org-heading-components)) org-done-keywords)))))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (let ((keyword (org-get-todo-state)))
            (when (and keyword
                       (not (member keyword (append '("LATER") org-done-keywords))))
              (setq has-subtask t)))))
      (and is-a-task has-subtask)))
)
(defun penguin/should-skip ()
  "Return non-nil if all the project subtasks are not stuck."
  (interactive)
  (let ((next-heading (save-excursion (or (outline-next-heading) (point-max))))
        (this-headline (org-element-at-point)))
    (save-restriction
      (save-excursion
        (org-narrow-to-subtree)
        (let* ((headlines (org-element-map (org-element-parse-buffer 'greater-element t)
                              'headline
                            #'identity))
               ;; Filter out element at point
               (direct-children (seq-filter (lambda (headline)
                                              (not (equal (org-element-property :begin this-headline)
                                                          (org-element-property :begin headline))))
                                            headlines)))
          (penguin/should-skip-children direct-children)))))
)
(defun penguin/should-skip-children (children)
  "Check if we should skip the CHILDREN."
  (widen)
  (if children
      ;; If there are any children, recursively see if any task in the subtree is stuck
      (if (seq-some (lambda (child)
                      (goto-char (org-element-property :begin child))
                      (not (penguin/should-skip)))
                    children)
          ;; If there are any task which should not be skipped, include it and continue.
          nil
        ;; Else we don't care about it
        next-heading)
    ;; No children, so check if it is a project task
    (if (penguin/find-project-task)
        (if (penguin/is-task-stuck)
            nil
          next-heading)
      next-heading))
)
(defun penguin-agenda-prefix ()
  "Define the prefix."
  (let* ((deadline (org-element-property :deadline (org-element-at-point)))
         (level (org-element-property :level (org-element-at-point)))
         (project-level (org-element-property :level (save-excursion
                                                       (bh/find-project-task t)
                                                       (org-element-at-point))))
         (adjusted (- level project-level))
         (category (org-entry-get (point) "CATEGORY")))
    (cond ((and deadline
                (not (bh/is-subproject-p)))
           (org-timestamp-format deadline "%x"))
          ((and (bh/is-subproject-p)
                (penguin/is-project-p))
           (penguin-agenda-make-prefix adjusted t))
          ((penguin/is-project-p)
           (concat category
                   ": "))
          (t (penguin-agenda-make-prefix adjusted)))
  )
)
(defun penguin-agenda-make-prefix (level &optional subproject-p)
  "Make the agenda prefix with LEVEL and the SUBPROJECT-P."
  (let ((adjusted (+ 11 level)))
    (concat (make-string adjusted ?\s)
            (char-to-string (org-bullets-level-char level))
            " "))
)
(defun bh/find-project-task (&optional top)
  "Move point to the parent (project) TOP task if any."
  (save-restriction
    (widen)
    (let ((this-task (save-excursion (org-back-to-heading 'invisible-ok) (point)))
          (parent-task))
      (while (and (or top (not parent-task)) (org-up-heading-safe))
        (when (bh/is-project-p)
          (setq parent-task (point))))
      (if parent-task
          (progn
            (goto-char parent-task)
            parent-task)
        (goto-char this-task)
        this-task)))
)
(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project."
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject))
)
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (not (member (nth 2 (org-heading-components)) org-done-keywords))))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (let ((keyword (org-get-todo-state)))
            (when (and keyword
                       (not (member keyword (append '("LATER") org-done-keywords))))
              (setq has-subtask t)))))
      (and is-a-task has-subtask)))
)

(provide 'setup_misc)
;;; setup_misc.el ends here
