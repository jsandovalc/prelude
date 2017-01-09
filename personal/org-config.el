;; Configuración del modo org
(global-set-key "\C-ca" 'org-agenda)

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/repositorio/notas-org/analisis.org"))))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
