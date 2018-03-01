(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s")

(add-hook 'web-mode-hook (lambda ()
                           (web-mode-set-engine "django")))

(add-hook 'prog-mode-hook 'flycheck-mode)

(add-to-list 'load-path "~/repos/html5-snippets")
(require 'html5-snippets)

(defun custom-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'custom-web-mode-hook)

(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-quoting t)


(setq js-indent-level 2)
