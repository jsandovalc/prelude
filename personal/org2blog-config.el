;; Prerequisites.
(prelude-require-package 'xml-rpc)
(prelude-require-package 'metaweblog)
(prelude-require-package 'org2blog)
(prelude-require-package 'htmlize)

(setq org2blog/wp-blog-alist
      '(("wordpress"
         :url "https://kishtablet.wordpress.com/xmlrpc.php"
         :username "ark4"
         :tags-as-categories nil)))
