;;; -*- lexical-binding: t; -*-

(provide 'evergreen-configure)

(require 'cl-lib)

(cl-defstruct evergreen-configure-variant display-name tasks collapsed)

(defun evergreen-configure-variant-parse (data)
  (make-evergreen-configure-variant
   :display-name (gethash "displayName" data)
   :tasks (gethash "tasks" data)
   :collapsed t
   ))

(defun evergreen-configure-patch (patch)
  (switch-to-buffer (get-buffer-create (format "evergreen-configure: %S" (alist-get 'description patch))))
  (read-only-mode -1)
  (evergreen-configure-patch-mode)
  (erase-buffer)
  (setq-local evergreen-configure-patch patch)
  (insert "Configure Patch")
  (newline)
  (insert (format "Description: %s" (alist-get 'description patch)))
  (newline 2)
  (setq-local evergreen-variants
              (seq-map
               (lambda (variant-data)
                 (let ((variant (evergreen-configure-variant-parse variant-data)))
                   (insert-text-button
                    (format "%s\n" (evergreen-configure-variant-display-name variant))
                    'variant variant)))
               (evergreen-get-patch-variants (alist-get 'patch_id patch)))
              )
  (read-only-mode)
  (goto-line 0)
  )

(defun evergreen-configure-current-variant ()
  (get-text-property (point) 'variant)
  )

(defun evergreen-configure-toggle-current-variant ()
  "Toggles the section at point"
  (interactive)
  (let ((variant (evergreen-configure-current-variant)) (initial-point (point)))
    (if (evergreen-configure-variant-collapsed variant)
        (progn (message "previously collapsed, expanding")
               (setf (evergreen-configure-variant-collapsed variant) nil)
               (read-only-mode -1)
               (forward-line)
               (seq-do
                (lambda (task) (insert (propertize (format "  %s" task))) (newline))
                (evergreen-configure-variant-tasks variant)
                )
               (read-only-mode -1))
      (message "previously not collapsed, collapsing")
      (setf (evergreen-configure-variant-collapsed variant) t)
      (next-line)
      (seq-do
       (lambda (task) (kill-whole-line))
       (evergreen-configure-variant-tasks variant)
       )
      )
    (goto-char initial-point)
    )
  )

(defvar evergreen-configure-mode-map nil "Keymap for evergreen-configure-patch page")

(progn
  (setq evergreen-configure-mode-map (make-sparse-keymap))

  (define-key evergreen-configure-patch-mode-map (kbd "<tab>") 'evergreen-configure-toggle-current-variant)

  (define-key evergreen-configure-patch-mode-map (kbd "r") (lambda ()
                                                             (interactive)
                                                             (evergreen-configure-patch evergreen-patch)))

  (define-key evergreen-configure-patch-mode-map (kbd "h") 'backward-char)
  (define-key evergreen-configure-patch-mode-map (kbd "j") 'next-line)
  (define-key evergreen-configure-patch-mode-map (kbd "k") 'previous-line)
  (define-key evergreen-configure-patch-mode-map (kbd "l") 'forward-char)
  )

(define-derived-mode
  evergreen-configure-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evergreen-configure buffer")
  
(evil-set-initial-state 'evergreen-configure-mode 'emacs)
