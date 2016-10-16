;; Configuración del modo org
(global-set-key "\C-ca" 'org-agenda)

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/repositorio/notas-org/analisis.org"))))
