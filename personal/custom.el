(set-face-attribute 'default nil :height 100)

(eval-after-load "dired-aux" '(add-to-list
  'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (whiteboard)))
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" default)))
 '(elfeed-feeds
   (quote
    ("http://django-planet.com/feeds/main/rss/" "http://planet.twistedmatrix.com/rss20.xml" "http://planetpython.org/rss20.xml")))
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (restclient elfeed yaml-mode dockerfile-mode inf-mongo rainbow-mode bbdb writeroom-mode fabric use-package rainbow-delimiters geiser company-auctex cdlatex auctex json-mode js2-mode key-chord company-anaconda anaconda-mode pygen edit-server markdown-mode php-mode zop-to-char zenburn-theme which-key web-mode volatile-highlights virtualenvwrapper undo-tree smartrep smartparens smart-mode-line projectile ox-reveal ov org2blog operate-on-number move-text monky magit imenu-anywhere htmlize guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region elpy easy-kill discover-my-major diminish diff-hl crux browse-kill-ring beacon anzu ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
