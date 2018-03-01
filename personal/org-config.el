;; Configuración del modo org
(require 'package)

(global-set-key "\C-ca" 'org-agenda)

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/notas-org/agenda.org"))))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

; new repo
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
