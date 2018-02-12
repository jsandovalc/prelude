;; TODO: Test with Cask (for pyvenv)
;; Get the $WORKON_HOME from pyvenv.

(defun custom-mkvirtualenv (directory)
  "Creates a python virtualenv in the ~/envs directory."
  (interactive (list (read-directory-name "Create virtualenv in: "
                                          custom-mkvirtualenv-default-directory)))
  (start-process "python-venv" "mkvenv-output" "python" "-m" "venv"
                 "--without-pip" directory)
  )

(defvar custom-mkvirtualenv-default-directory "/home/ark/envs/"
  "Initial starting point")
