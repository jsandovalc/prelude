(setq erc-autojoin-channels-alist
      '(("freenode.net" "#mediagoblin" )))

(yas-global-mode 1)

;; Swiper is too slow on big files like my org. Use counsel-grep
;; instead.
(defun ora-swiper ()
  (interactive)
  (if (and (buffer-file-name)
           (not (ignore-errors
                  (file-remote-p (buffer-file-name))))
           (if (eq major-mode 'org-mode)
               (> (buffer-size) 60000)
             (> (buffer-size) 300000)))
      (progn
        (save-buffer)
        (counsel-grep))
    (swiper--ivy (swiper--candidates))))

(global-set-key "\C-s" 'ora-swiper)

;; hideshow for programming
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Recompile
(global-set-key (quote [f5]) 'recompile)
