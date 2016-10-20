(prelude-require-package 'elpy)

(elpy-enable)
(elpy-use-ipython)
;; Configure flymake for Python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; Set as a minor mode for Python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))

(defun prelude-python--encoding-comment-required-p ()
  nil)


;; temporary solution to run pylint in a project.  requires a
;; __init__.py at the base level.  As there are errors. Instead of
;; doing it like thise, the modules must be listed.

;; (defun elpy-check (&optional whole-project-p)
;;   "Run `python-check-command' on the current buffer's file,
;; or the project root if WHOLE-PROJECT-P is non-nil (interactively,
;; with a prefix argument)."
;;   (interactive "P")
;;   (when (not (buffer-file-name))
;;     (error "Can't check a buffer without a file."))
;;   (save-some-buffers (not compilation-ask-about-save) nil)
;;   (let ((process-environment (python-shell-calculate-process-environment))
;;         (exec-path (python-shell-calculate-exec-path))
;;         (file-name-or-directory (expand-file-name
;;                                  (if whole-project-p
;;                                      (or (elpy-project-root)
;;                                          (buffer-file-name))
;;                                    (buffer-file-name))))
;;         (extra-args (if whole-project-p
;;                         (concat " --ignore="
;;                                 (mapconcat #'identity
;;                                            elpy-project-ignored-directories
;;                                            ","))
;;                       "")))
;;     (compilation-start (concat python-check-command
;;                                " "
;;                                (shell-quote-argument file-name-or-directory)
;;                                extra-args)
;;                        nil
;;                        (lambda (mode-name)
;;                          "*Python Check*"))))
