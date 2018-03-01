(require 'notmuch)

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(require 'org-notmuch)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "cloudneozero@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      starttls-gnutls-program "/usr/bin/gnutls-cli"
      starttls-extra-arguments nil
      starttls-use-gnutls t)
