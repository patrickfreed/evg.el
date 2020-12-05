;;; -*- lexical-binding: t; -*-

(provide 'evergreen-configure)

(require 'cl-lib)

(cl-defstruct evergreen-configure-variant display-name name tasks collapsed location)

(defun evergreen-configure-variant-nselected-tasks (variant)
  (length (evergreen-configure-variant-selected-tasks variant)))

(defun evergreen-configure-variant-selected-tasks (variant)
  (seq-filter
   'evergreen-configure-task-selected
   (evergreen-configure-variant-tasks variant)))

(cl-defstruct evergreen-configure-task name selected location parent)

(defun evergreen-configure-task-is-visible (task)
  (not (evergreen-configure-variant-collapsed (evergreen-configure-task-parent task))))

(defun evergreen-configure-variant-parse (data)
  (let
      ((variant
        (make-evergreen-configure-variant
         :display-name (gethash "displayName" data)
         :name (gethash "name" data)
         :tasks (seq-map (lambda (task-name)
                           (make-evergreen-configure-task :name task-name :selected nil :location nil :parent nil))
                         (gethash "tasks" data))
         :collapsed t
         :location nil
         )))
    (seq-do
     (lambda (task)
       (setf (evergreen-configure-task-parent task) variant))
     (evergreen-configure-variant-tasks variant))
    variant))

(defun evergreen-configure-variant-update-nselected (variant)
  (let ((initial-point (point)))
    (read-only-mode -1)
    (goto-char (marker-position (evergreen-configure-variant-location variant)))
    (if (search-forward-regexp "([0-9]+/[0-9]+)$")
        (replace-match
         (format
          "(%d/%d)"
          (evergreen-configure-variant-nselected-tasks variant)
          (length (evergreen-configure-variant-tasks variant)))
         ))
    (read-only-mode)
    (goto-char initial-point))
  )

(defun evergreen-configure-patch (patch)
  (switch-to-buffer (get-buffer-create (format "evergreen-configure: %S" (alist-get 'description patch))))
  (read-only-mode -1)
  (evergreen-configure-mode)
  (erase-buffer)
  (setq-local evergreen-patch patch)
  (insert "Configure Patch")
  (newline)
  (insert (format "Description: %s" (alist-get 'description patch)))
  (newline 2)
  (setq-local evergreen-variants
              (seq-map
               (lambda (variant-data)
                 (let ((variant (evergreen-configure-variant-parse variant-data)))
                   (setf (evergreen-configure-variant-location variant) (point-marker))
                   (insert-text-button
                    (evergreen-configure-variant-display-name variant)
                    'evergreen-configure-variant variant)
                   (insert-and-inherit
                    (format
                     " (%d/%d)"
                     (evergreen-configure-variant-nselected-tasks variant)
                     (length (evergreen-configure-variant-tasks variant))))
                   (newline)
                   variant))
               (evergreen-get-patch-variants (alist-get 'patch_id patch)))
              )
  (read-only-mode)
  (goto-line 0)
  )

(defun evergreen-configure-current-variant ()
  (get-text-property (point) 'evergreen-configure-variant)
  )

(defun evergreen-configure-current-task ()
  (get-text-property (point) 'evergreen-configure-task)
  )

(defun evergreen-configure-toggle-current-variant ()
  "Toggles the section at point"
  (interactive)
  (read-only-mode -1)
  (let ((variant (evergreen-configure-current-variant)) (initial-point (point)))
    (if (evergreen-configure-variant-collapsed variant)
        (progn
          (setf (evergreen-configure-variant-collapsed variant) nil)
          (forward-line)
          (seq-do
           (lambda (task)
             (setf (evergreen-configure-task-location task) (point-marker))
             (insert (with-temp-buffer
                       (if (evergreen-configure-task-selected task)
                           (insert "[x] ")
                         (insert "[ ] "))
                       (insert (format "%s" (evergreen-configure-task-name task)))
                       (put-text-property (point-min) (point-max) 'evergreen-configure-task task)
                       (buffer-string)
                       ))
             (newline))
           (evergreen-configure-variant-tasks variant)
           )
          )
      (setf (evergreen-configure-variant-collapsed variant) t)
      (next-line)
      (seq-do
       (lambda (task)
         (setf (evergreen-configure-task-location task) nil)
         (kill-whole-line))
       (evergreen-configure-variant-tasks variant)
       )
      )
    (goto-char initial-point)
    (read-only-mode)
    )
  )

(defun evergreen-configure-select-task (task)
  "Toggles whether the provided task is selcted or not. This must be called when the point is on
   the same line as the task."
  (let ((initial-point (point)) (is-selected (evergreen-configure-task-selected task)))
    (setf (evergreen-configure-task-selected task) (not is-selected))
    (if-let ((location (evergreen-configure-task-location task)))
        (progn
          (read-only-mode -1)
          (goto-char (marker-position location))
          (forward-char)
          (delete-char 1)
          (if is-selected
              (progn
                (insert-and-inherit " "))
            (insert-and-inherit "x"))
          (goto-char initial-point)
          (next-line)
          (read-only-mode))
      )
    (evergreen-configure-variant-update-nselected (evergreen-configure-task-parent task))
    )
  )

(defun evergreen-configure-select-at-point ()
  "Toggles the section at point"
  (interactive)
  (read-only-mode -1)
  (if-let ((task (evergreen-configure-current-task)))
      (evergreen-configure-select-task task)
    (if-let ((variant (evergreen-configure-current-variant)))
        (progn
          (seq-do 'evergreen-configure-select-task (evergreen-configure-variant-tasks variant))
          (next-line))
      )
    )
  )

(defun evergreen-configure-schedule ()
  (interactive)
  (if-let
      ((selected-variants
        (seq-filter
         (lambda (variant) (> (evergreen-configure-variant-nselected-tasks variant) 0))
         evergreen-variants)))
      (progn
        (message "Scheduling patch...")
        (evergreen-api-post
         (format "patches/%s/configure" (alist-get 'patch_id evergreen-patch))
         (lambda (_) (evergreen-view-patch-data evergreen-patch))
         (json-encode
          (list
           (cons "description" (alist-get 'description evergreen-patch))
           (cons
            "variants"
            (seq-map
             (lambda (variant)
               (list
                (cons "id" (evergreen-configure-variant-name variant))
                (cons "tasks"
                      (seq-map 'evergreen-configure-task-name (evergreen-configure-variant-selected-tasks variant)))))
             selected-variants))))))))

(defvar evergreen-configure-mode-map nil "Keymap for evergreen-configure buffers")

(progn
  (setq evergreen-configure-mode-map (make-sparse-keymap))

  (define-key evergreen-configure-mode-map (kbd "<tab>") 'evergreen-configure-toggle-current-variant)
  (define-key evergreen-configure-mode-map (kbd "m") 'evergreen-configure-select-at-point)
  (define-key evergreen-configure-mode-map (kbd "x") 'evergreen-configure-schedule)

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
