(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote eww-browse-url))
 '(browse-url-generic-program nil)
 '(elfeed-feeds
   (quote
    ("http://dalwiki.derechoaleer.org/recentchanges/index.atom" "http://feeds.feedburner.com/ElLadoDelMal" "http://planet.emacsen.org/atom.xml" "http://rss.slashdot.org/Slashdot/slashdot" "http://feeds.feedburner.com/ENTERCO")) t)
 '(elpy-test-django-runner-command (quote ("./manage.py" "test" "--noinput")))
 '(elpy-test-django-runner-manage-command (quote ("./manage.py" "test" "--noinput")))
 '(elpy-test-runner (quote elpy-test-django-runner))
 '(magit-remote-arguments (quote ("-f")))
 '(notmuch-address-command "/home/ark/src/notmuch-addrlookup-c/notmuch-addrlookup")
 '(package-selected-packages
   (quote
    (vue-mode markdown-mode rjsx-mode use-package pipenv elfeed pdf-tools org2blog metaweblog xml-rpc notmuch web-mode virtualenvwrapper org org-plus-contrib counsel ox-reveal elpy monky swiper yasnippet zop-to-char zenburn-theme which-key volatile-highlights undo-tree smartrep smartparens smart-mode-line projectile ov operate-on-number move-text magit ivy imenu-anywhere guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region editorconfig easy-kill discover-my-major diminish diff-hl crux browse-kill-ring beacon anzu ace-window)))
 '(pyvenv-mode t)
 '(pyvenv-workon nil)
 '(send-mail-function (quote sendmail-send-it))
 '(smtpmail-default-smtp-server "smtp.gmail.com" t)
 '(smtpmail-smtp-service "587" t)
 '(smtpmail-smtp-user "cloudneozero@gmail.com")
 '(smtpmail-stream-type (quote starttls))
 '(starttls-gnutls-program "/usr/bin/gnutls-cli")
 '(starttls-use-gnutls t))
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
