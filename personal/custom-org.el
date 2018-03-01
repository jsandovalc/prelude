(setq org-directory "~/notas-org")
(setq org-list-allow-alphabetical t)

(setq org-default-notes-file (concat org-directory "/agenda.org"))

(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "" "Tasks")
         "* TODO %?\n %t\n %a\n %i")
        ("c" "Tarea de la Corriente" entry (file+headline "" "Corriente")
         "* TODO %?\n %t\n %a\n %i")
        ("f" "New facturedo task" entry
         (file+olp "" "Facturedo" "Tareas")
         "*** TODO %?\n %i\n %a" :jump-to-captured t :empty-lines 1)))
