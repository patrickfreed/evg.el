;;; -*- lexical-binding: t; -*-

(provide 'evergreen-configure)

(require 'cl-lib)

(cl-defstruct evergreen-configure-variant display-name tasks collapsed)
(cl-defstruct evergreen-configure-task name selected)

(defun evergreen-configure-variant-parse (data)
  (make-evergreen-configure-variant
   :display-name (gethash "displayName" data)
   :tasks (seq-map (lambda (task-name)
                     (make-evergreen-configure-task :name task-name :selected nil))
                   (gethash "tasks" data))
   :collapsed t
   ))

(defun evergreen-configure-patch (patch)
  (switch-to-buffer (get-buffer-create (format "evergreen-configure: %S" (alist-get 'description patch))))
  (read-only-mode -1)
  (evergreen-configure-mode)
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
  (read-only-mode -1)
  (let ((variant (evergreen-configure-current-variant)) (initial-point (point)))
    (if (evergreen-configure-variant-collapsed variant)
        (progn
          (message "previously collapsed, expanding")
          (setf (evergreen-configure-variant-collapsed variant) nil)
          (forward-line)
          (seq-do
           (lambda (task)
             (insert (with-temp-buffer
                       (if (evergreen-configure-task-selected task)
                           (insert "[x] ")
                         (insert "[ ] "))
                       (insert (format "%s" (evergreen-configure-task-name task)))
                       (put-text-property (point-min) (point-max) 'task task)
                       (buffer-string)
                       ))
             (newline))
           (evergreen-configure-variant-tasks variant)
           )
          )
      (message "previously not collapsed, collapsing")
      (setf (evergreen-configure-variant-collapsed variant) t)
      (next-line)
      (seq-do
       (lambda (task) (kill-whole-line))
       (evergreen-configure-variant-tasks variant)
       )
      )
    (goto-char initial-point)
    (read-only-mode)
    )
  )

(defun evergreen-configure-select-current-task ()
  "Toggles the section at point"
  (interactive)
  (read-only-mode -1)
  (let ((task (get-text-property (point) 'task)) (initial-point (point)))
    (if (evergreen-configure-task-selected task)
        (progn
          (message "previously selected, unselecting")
          (setf (evergreen-configure-task-selected task) nil)
          (beginning-of-line)
          (forward-char)
          (delete-char 1)
          (insert " ")
          (goto-char initial-point)
          )
      (message "previously not selected, selecting")
      (setf (evergreen-configure-task-selected task) t)
      (beginning-of-line)
      (forward-char)
      (delete-char 1)
      (insert "x")
      (goto-char initial-point)
      (next-line)
      )
    (read-only-mode)
    )
  )

(defvar evergreen-configure-mode-map nil "Keymap for evergreen-configure buffers")

(progn
  (setq evergreen-configure-mode-map (make-sparse-keymap))

  (define-key evergreen-configure-mode-map (kbd "<tab>") 'evergreen-configure-toggle-current-variant)
  (define-key evergreen-configure-mode-map (kbd "m") 'evergreen-configure-select-current-task)

  (define-key evergreen-configure-mode-map (kbd "r") (lambda ()
                                                             (interactive)
                                                             (evergreen-configure-patch evergreen-patch)))

  (define-key evergreen-configure-mode-map (kbd "h") 'backward-char)
  (define-key evergreen-configure-mode-map (kbd "j") 'next-line)
  (define-key evergreen-configure-mode-map (kbd "k") 'previous-line)
  (define-key evergreen-configure-mode-map (kbd "l") 'forward-char)
  )

(define-derived-mode
  evergreen-configure-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evergreen-configure buffer")
  
(evil-set-initial-state 'evergreen-configure-mode 'emacs)
