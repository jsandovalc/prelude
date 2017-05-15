(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-syntax-check-command "pyflakes")
 '(js-indent-level 2)
 '(magit-popup-use-prefix-argument (quote default))
 '(package-selected-packages
   (quote
    (yaml-mode stylus-mode inf-mongo php-mode zop-to-char zenburn-theme which-key web-mode volatile-highlights virtualenvwrapper undo-tree typing tomatinho swiper speed-type smex smartrep smartparens smart-mode-line restclient rainbow-mode rainbow-delimiters pomodoro ox-reveal ov org2blog operate-on-number move-text monky markdown-mode magit log4j-mode key-chord json-rpc json-mode js2-mode imenu-anywhere ido-ubiquitous htmlize helm-projectile helm-descbinds helm-ag guru-mode grizzl gradle-mode god-mode gitlab gitignore-mode gitconfig-mode git-timemachine gist flycheck flx-ido expand-region elpy elisp-slime-nav elfeed easy-kill discover-my-major diminish diff-hl csv-mode crux company-auctex company-anaconda cdlatex browse-kill-ring beacon anzu android-mode ace-window ace-jump-mode ace-jump-buffer)))
 '(projectile-project-root-files
   (quote
    ("rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "setup.py" "tox.ini" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" "run.py")))
 '(python-check-command "pylint"))
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
