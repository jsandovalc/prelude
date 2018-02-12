(prelude-require-package 'elpy)

(elpy-enable)
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

(setq projectile-completion-system 'ivy)

;; virtualenv
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; of course I want eshell support

(setq eshell-prompt-function
      (lambda ()
        (concat venv-current-name " $ ")))
