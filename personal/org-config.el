;; Configuración del modo org
(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/org/org-agenda/agenda.org"
                      "~/org/org-agenda/ingresos.org"
                      "~/org/org-agenda/movimientos.org"
                      "~/org/org-agenda/personal.org"))))
