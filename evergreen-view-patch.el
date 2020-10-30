;;; -*- lexical-binding: t; -*-

(provide 'evergreen-view-patch)

(defun evergreen-view-patch (patch)
  (switch-to-buffer (get-buffer-create (format "evergreen-view-patch: %S" (alist-get 'description patch))))
  (read-only-mode -1)
  (evergreen-view-patch-mode)
  (erase-buffer)
  (setq-local evergreen-patch patch)
  (insert (format "Patch %d: %s" (alist-get 'patch_number patch) (alist-get 'description patch)))
  (newline)
  (insert (format "Status: %s" (alist-get 'status patch)))
  (newline)
  (insert (format "Created at: %s" (alist-get 'create_time patch)))
  (newline)
  (newline 2)
  (read-only-mode)
  (goto-line 0)
  )

(defvar evergreen-view-patch-mode-map nil "Keymap for evergreen-view-patch buffers")

(progn
  (setq evergreen-view-patch-mode-map (make-sparse-keymap))

  (define-key evergreen-view-patch-mode-map (kbd "r") (lambda ()
                                                             (interactive)
                                                             (evergreen-view-patch evergreen-patch)))

  (define-key evergreen-view-patch-mode-map (kbd "h") 'backward-char)
  (define-key evergreen-view-patch-mode-map (kbd "j") 'next-line)
  (define-key evergreen-view-patch-mode-map (kbd "k") 'previous-line)
  (define-key evergreen-view-patch-mode-map (kbd "l") 'forward-char)
  )

(define-derived-mode
  evergreen-view-patch-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evergreen-view-patch buffer")
  
(evil-set-initial-state 'evergreen-view-patch-mode 'emacs)
