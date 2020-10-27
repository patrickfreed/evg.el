(defun evergreen-patch (project-name description)
  "Submit a patch to the given project with the given description. Returns the patch's ID."
  (let (output command)
    (setq command (format "evergreen patch -p \"%s\" --yes --description \"%s\"" project-name description))
    (setq output (shell-command-to-string command))
    (string-match "ID : \\([a-f0-9]+\\)" output)
    (let (id)
      (setq id (match-string 1 output))
      (if (not id) (message (format "patch failed: %s" output)))
      id)))

(evergreen-patch "mongo-rust-driver" "test from elisp function")
