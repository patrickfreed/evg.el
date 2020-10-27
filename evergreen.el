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

(defun evergreen-status (project-name)
  "Open the evergreen status page for the given project"
  (let (status-buffer)
    (setq status-buffer (get-buffer-create (format "evergreen-status: %s" project-name)))
    (switch-to-buffer status-buffer)
    (erase-buffer)
    (insert (format "evergreen project: %s" project-name))
    (read-only-mode)))

(defun evergreen-status-debug ()
  (interactive)
  (evergreen-status "foo"))

