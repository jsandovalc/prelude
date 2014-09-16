;; Prerequisites.
(prelude-require-package 'xml-rpc)
(prelude-require-package 'metaweblog)
(prelude-require-package 'org2blog)
(prelude-require-package 'htmlize)

(setq org-directory "~/personal/org-agenda/")
(setq org-list-allow-alphabetical t)

(require 'org2blog-autoloads)
(require 'netrc)

; (setq wordpress (netrc-machine (netrc-parse "~/.netrc") "api.heroku.com" t))

(setq org2blog/wp-blog-alist
      '(("wordpress"
         :url "http://conciencialibreblog.wordpress.com/xmlrpc.php"
         :username "jsandoval@utp.edu.co"
         :default-title "Hello World"
         :default-categories ("python" "emacs")
         :tags-as-categories nil)))
(setq org2blog/wp-use-sourcecode-shortcode nil) ;; 't if I want not native emacs.
(setq org2blog/wp-sourcecode-default-params nil)
(setq org2blog/wp-sourcecode-langs
      '("actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
        "erlang" "fsharp" "diff" "groovy" "javascript" "java" "javafx" "matlab"
        "objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql"
        "vb" "xml"
        "sh" "emacs-lisp" "lisp" "lua"))
(setq org-src-fontify-natively t)
