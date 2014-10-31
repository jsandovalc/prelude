(prelude-require-package 'virtualenvwrapper)

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "~/envs/")
