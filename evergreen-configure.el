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

(defun evergreen-configure-goto-thing ()
  (interactive)
  (if-let ((task (evergreen-configure-current-task)))
      (goto-char (marker-position (evergreen-configure-task-location task)))
    (if-let ((variant (evergreen-configure-current-variant)))
      (goto-char (marker-position (evergreen-configure-variant-location variant)))
      )
    )
  )

(defun evergreen-configure-variant-face (variant)
  (let ((nselected (evergreen-configure-variant-nselected-tasks variant)))
    (if (> nselected 0)
        (if (= nselected (length (evergreen-configure-variant-tasks variant)))
            'success
          'warning)
      '('evergreen-configure-variant-unselected . 'bold))))

(defun evergreen-configure-variant-insert (variant)
  (insert
   (propertize
    (concat
     (if (evergreen-configure-variant-collapsed variant) "⮞" "⮟")
     " "
     (evergreen-configure-variant-display-name variant)
     (format
      " (%d/%d)"
      (evergreen-configure-variant-nselected-tasks variant)
      (length (evergreen-configure-variant-tasks variant))))
    'evergreen-configure-variant variant
    'face (evergreen-configure-variant-face variant)
    'rear-nonsticky t))
  (setf (evergreen-configure-variant-location variant) (evergreen-configure-make-marker)))

(cl-defstruct evergreen-configure-task name selected location parent)

(defun evergreen-configure-task-is-visible (task)
  (not (evergreen-configure-variant-collapsed (evergreen-configure-task-parent task))))

(defun evergreen-configure-task-face (task)
  (if (evergreen-configure-task-selected task)
      'evergreen-configure-task-selected
    'evergreen-configure-variant-unselected))

(defun evergreen-configure-task-insert (task)
  (insert
   (with-temp-buffer
     (insert "  ")
     (evergreen-configure-insert-checkbox (evergreen-configure-task-selected task))
     (insert " ")
     (insert (evergreen-configure-task-name task))
     (add-text-properties (point-min) (point-max) (list 'evergreen-configure-task task))
     (add-face-text-property (point-min) (point-max) (evergreen-configure-task-face task))
     (buffer-string)))
  (setf (evergreen-configure-task-location task) (evergreen-configure-make-marker)))

(defun evergreen-configure-variant-parse (data scheduled-tasks)
  "Parse a configure-variant from the given data, using the provided alist of display-name to evergreen-task-info
   to determine pre-selected tasks."
  (let*
      ((variant-scheduled-tasks (or (cdr (assoc-string (gethash "displayName" data) scheduled-tasks)) '()))
       (variant
        (make-evergreen-configure-variant
         :display-name (gethash "displayName" data)
         :name (gethash "name" data)
         :tasks (seq-map (lambda (task-name)
                           (make-evergreen-configure-task
                            :name task-name
                            :selected (seq-some
                                       (lambda (task)
                                         (string= task-name (evergreen-task-info-display-name task)))
                                       variant-scheduled-tasks)
                            :location nil
                            :parent nil))
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
  (save-excursion 
    (read-only-mode -1)
    (goto-char (marker-position (evergreen-configure-variant-location variant)))
    (kill-line)
    (evergreen-configure-variant-insert variant)
    (read-only-mode)))

(defun evergreen-configure-make-marker ()
  (let ((marker (make-marker)))
    (set-marker marker (line-beginning-position))
    (set-marker-insertion-type marker t)
    marker))

(defun evergreen-configure-insert-checkbox (is-selected)
  (insert
   (propertize
    (concat "[" (if is-selected "✔" " ") "]")
    'face 'org-checkbox)))

(defun evergreen-configure-patch-data (patch-data)
  (evergreen-configure-patch (evergreen-patch-parse patch-data) '()))

(defun evergreen-configure-patch (patch scheduled-tasks)
  "Switch to a configuration buffer for the given evergreen-patch struct, using the provided alist of display-name
   to evergreen-task-info to determine pre-scheduled tasks"
  (switch-to-buffer (get-buffer-create (format "evergreen-configure: %S" (evergreen-patch-description patch))))
  (read-only-mode -1)
  (evergreen-configure-mode)
  (erase-buffer)
  (setq-local evergreen-patch patch)
  ;; (insert "Configure Patch")

  (evergreen-ui-insert-header
   (list
    (cons "Description" (evergreen-patch-description patch))
    (cons "Patch Number" (number-to-string (evergreen-patch-number patch)))
    (cons "Status" (evergreen-status-text (evergreen-patch-status patch)))
    (cons "Created at" (evergreen-date-string (evergreen-patch-create-time patch))))
   "Configure Patch")
  ;; (newline)
  ;; (insert (format "Description: %s" (evergreen-patch-description patch)))
  (newline)
  (setq-local evergreen-variants
              (seq-map
               (lambda (variant-data)
                 (let ((variant (evergreen-configure-variant-parse variant-data scheduled-tasks)))
                   (evergreen-configure-variant-insert variant)
                   (newline)
                   variant))
               (evergreen-get-patch-variants (evergreen-patch-id patch)))
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
  (when-let ((variant (evergreen-configure-current-variant)))
    (read-only-mode -1)
    (save-excursion
      (setf (evergreen-configure-variant-collapsed variant) (not (evergreen-configure-variant-collapsed variant)))
      (if (evergreen-configure-variant-collapsed variant)
          (progn
            (next-line)
            (seq-do
             (lambda (task)
               (setf (evergreen-configure-task-location task) nil)
               (kill-whole-line))
             (evergreen-configure-variant-tasks variant)))
        (forward-line)
        (seq-do
         (lambda (task)
           (evergreen-configure-task-insert task)
           (newline))
         (evergreen-configure-variant-tasks variant)))
      (goto-char (marker-position (evergreen-configure-variant-location variant)))
      (kill-line)
      (evergreen-configure-variant-insert variant))
    (read-only-mode)
    )
  )

(defun evergreen-configure-select-task (task)
  "Toggles whether the provided task is selcted or not. This must be called when the point is on
   the same line as the task."
  (let ((is-selected (evergreen-configure-task-selected task)))
    (setf (evergreen-configure-task-selected task) (not is-selected))
    (when-let ((location (evergreen-configure-task-location task)))
        (save-excursion
          (read-only-mode -1)
          (goto-char (marker-position location))
          (kill-line)
          (evergreen-configure-task-insert task)
          (read-only-mode))
        (next-line)
      )
    (evergreen-configure-variant-update-nselected (evergreen-configure-task-parent task))
    )
  )

(defun evergreen-configure-select-at-point ()
  "Toggles the section at point"
  (interactive)
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
         (format "patches/%s/configure" (evergreen-patch-id evergreen-patch))
         (lambda (_) (evergreen-view-patch evergreen-patch))
         (json-encode
          (list
           (cons "description" (evergreen-patch-description evergreen-patch))
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
  (define-key evergreen-configure-mode-map (kbd "g") 'evergreen-configure-goto-thing)

  (define-key evergreen-configure-mode-map (kbd "r") (lambda ()
                                                             (interactive)
                                                             (evergreen-configure-patch evergreen-patch '())))

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

(defface evergreen-configure-variant-unselected
  '((t (:inherit 'shadow)))
  "The face to use for variants that have no selected tasks")

(defface evergreen-configure-task-selected
  '((t (:inherit 'success :bold nil)))
  "The face to use for a task that has been selected")
