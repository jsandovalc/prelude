;; (use-package ivy :ensure t
;;   :diminish (ivy-mode . "")
;;   :bind
;;   (:map ivy-mode-map
;;    ("C-'" . ivy-avy))
;;   :config
;;   (ivy-mode 1)
;;   ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
;;   (setq ivy-use-virtual-buffers t)
;;   ;; number of result lines to display
;;   (setq ivy-height 10)
;;   ;; does not count candidates
;;   (setq ivy-count-format "")
;;   ;; no regexp by default
;;   (setq ivy-initial-inputs-alist nil)
;;   (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;   (global-set-key (kbd "<f6>") 'ivy-resume)
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;   (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;   (global-set-key (kbd "<f1> l") 'counsel-find-library)
;;   (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;   (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;   (global-set-key (kbd "C-c g") 'counsel-git)
;;   (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;   (global-set-key (kbd "C-c k") 'counsel-ag)
;;   (global-set-key (kbd "C-x l") 'counsel-locate)
;;   (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;   (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;;   ;; configure regexp engine.
;;   (global-set-key "\C-s" 'swiper)
;;   (setq ivy-re-builders-alist
;; 	;; allow input not in order
;;         '((t   . ivy--regex-ignore-order))))

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
;; number of result lines to display
(setq ivy-height 10)
;; does not count candidates
(setq ivy-count-format "")
;; no regexp by default
(setq ivy-initial-inputs-alist nil)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
(global-set-key "\C-s" 'swiper)
(setq ivy-re-builders-alist
      ;; allow input not in order
      '((t   . ivy--regex-ignore-order)))
