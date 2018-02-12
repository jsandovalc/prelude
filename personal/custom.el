(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(browse-url-browser-function (quote eww-browse-url))
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (smart-mode-line-light)))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(elfeed-feeds
   (quote
    ("http://www.eltiempo.com/rss/salud.xml" "http://www.eltiempo.com/rss/medio-ambiente.xml" "http://www.eltiempo.com/rss/ciencia.xml" "http://www.eltiempo.com/rss/educaci%C3%B3n.xml" "http://www.eltiempo.com/rss/novedades-tecnologia.xml" "http://www.eltiempo.com/rss/tecnosfera.xml" "http://www.eltiempo.com/rss/cultura.xml" "http://www.eltiempo.com/rss/sector-financiero.xml" "http://www.eltiempo.com/rss/sectores.xml" "http://www.eltiempo.com/rss/empresas.xml" "http://www.eltiempo.com/rss/economia.xml" "http://www.eltiempo.com/rss/conflicto-y-narcotrafico.xml" "http://www.eltiempo.com/rss/cortes.xml" "http://www.eltiempo.com/rss/justicia.xml" "http://www.eltiempo.com/rss/partidos-politicos.xml" "http://www.eltiempo.com/rss/proceso-de-paz.xml" "http://www.eltiempo.com/rss/congreso.xml" "http://www.eltiempo.com/rss/gobierno.xml" "http://www.eltiempo.com/rss/politica.xml" "http://www.eltiempo.com/rss/mas-regiones.xml" "http://www.eltiempo.com/rss/africa.xml" "http://www.eltiempo.com/rss/asia.xml" "http://www.eltiempo.com/rss/medio-oriente.xml" "http://www.eltiempo.com/rss/europa.xml" "http://www.eltiempo.com/rss/eeuu-y-canada.xml" "http://www.eltiempo.com/rss/latinoamerica.xml" "http://www.eltiempo.com/rss/mundo.xml" "http://www.eltiempo.com/rss/otras-ciudades.xml" "http://www.eltiempo.com/rss/colombia.xml" "http://www.eltiempo.com/rss/mas-opinion.xml" "http://www.eltiempo.com/rss/editorial.xml" "http://www.eltiempo.com/rss/opinion.xml" "http://www.espectador.com/rss/internacionales.xml" "http://www.espectador.com/rss/sociedad.xml" "http://www.espectador.com/rss/salud.xml" "http://www.espectador.com/rss/politica.xml" "http://www.espectador.com/rss/medioambiente.xml" "http://www.espectador.com/rss/economia.xml" "http://www.espectador.com/rss/cultura.xml" "http://www.espectador.com/rss/agro.xml" "http://www.espectador.com/rss/cienciaytecnologia.xml" "http://feeds.mashable.com/Mashable" "http://feeds.feedburner.com/AtheistMedia?format=xml" "http://www.cynical-c.com/feed/" "https://www.popsci.com/rss-environment.xml?loc=contentwell&lnk=environment&dom=section-1" "https://www.popsci.com/rss-space.xml?loc=contentwell&lnk=space&dom=section-1" "https://www.popsci.com/rss-technology.xml?loc=contentwell&lnk=tech&dom=section-1" "https://www.popsci.com/rss-science.xml?loc=contentwell&lnk=science&dom=section-1" "}" "https://lwn.net/headlines/rss" "http://planet.twistedmatrix.com/atom.xml" "https://static.fsf.org/fsforg/rss/news.xml" "http://django-planet.com/feeds/main/rss/" "http://planetpython.org/rss20.xml" "http://planet.emacsen.org/atom.xml")))
 '(elpy-django-command "./manage.py")
 '(elpy-test-django-runner-command (quote ("./manage.py" "test")))
 '(fci-rule-color "#383838")
 '(mail-user-agent (quote notmuch-user-agent))
 '(message-send-mail-function (quote message-smtpmail-send-it))
 '(notmuch-address-command "/home/ark/src/notmuch-addrlookup-c/notmuch-addrlookup")
 '(notmuch-address-use-company t)
 '(notmuch-message-replied-tags (quote ("+replied")))
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "Jaime" :query "tag:blog")
     (:name "Itamar" :query "tag:itamar"))))
 '(notmuch-search-oldest-first nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org2blog/wp-default-categories (quote ("Uncategorized")))
 '(package-selected-packages
   (quote
    (org-gcal origami counsel-etags json-navigator ghub magithub use-package org-plus-contrib counsel swiper transmission pdf-tools vlf inf-mongo elfeed elfeed-goodies elfeed-web markdown-mode company-restclient restclient virtualenvwrapper ox-reveal elpy org2blog metaweblog xml-rpc monky key-chord zop-to-char zenburn-theme which-key volatile-highlights undo-tree smartrep smartparens smart-mode-line operate-on-number move-text magit projectile ov imenu-anywhere guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region epl editorconfig easy-kill diminish diff-hl discover-my-major dash crux browse-kill-ring beacon anzu ace-window)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pyvenv-mode t)
 '(pyvenv-tracking-mode t)
 '(send-mail-function (quote smtpmail-send-it))
 '(shr-external-browser (quote browse-url-firefox))
 '(smtpmail-default-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-stream-type (quote ssl))
 '(user-full-name "Jonathan Sandoval")
 '(user-mail-address "cloudneozero@gmail.com")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
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
