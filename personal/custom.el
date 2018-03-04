(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zop-to-char zenburn-theme yaml-mode which-key web-mode vue-mode volatile-highlights virtualenvwrapper use-package undo-tree smex smartrep smartparens smart-mode-line scss-mode rjsx-mode rainbow-mode rainbow-delimiters racer pipenv pdf-tools ox-reveal ov org2blog org-plus-contrib operate-on-number notmuch move-text monky markdown-mode magit macrostep key-chord json-mode imenu-anywhere ido-completing-read+ guru-mode grizzl gotest god-mode go-projectile gitignore-mode gitconfig-mode git-timemachine gist flycheck-rust flx-ido expand-region ensime elpy elisp-slime-nav elfeed editorconfig easy-kill discover-my-major diminish diff-hl crux counsel company-go company-auctex company-anaconda cider cdlatex cargo browse-kill-ring beacon anzu ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(set-face-attribute 'default nil :height 100)

(eval-after-load "dired-aux" '(add-to-list
  'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))



(add-hook 'web-mode-hook (lambda ()
                           (web-mode-set-engine "django")))

(global-undo-tree-mode 0)
