;;; -*- lexical-binding: t; -*-

(provide 'evergreen-grid)

(require 'cl-lib)

(cl-defstruct evergreen-grid-element description status data)

(defface evergreen-grid-success
  `((t
     :background ,(face-attribute 'success :foreground)
     :foreground "black"
     :box t))
  "success evergreen grid face")

(defface evergreen-grid-failed
  `((t
     :background ,(face-attribute 'error :foreground)
     :foreground "black"
     :box t))
  "fail evergreen grid face")

(defface evergreen-grid-started
  `((t
     :background ,(face-attribute 'warning :foreground)
     :foreground "black"
     :box t))
  "in progress evergreen grid face")

(defface evergreen-grid-system-failure
  '((t
     :background "#800080"
     :foreground "black"
     :box t))
  "system failure evergreen grid face")

(defface evergreen-grid-undispatched
  '((t
     :background "#bfbfbe"
     :foreground "black"
     :box t))
  "undispatched evergreen grid face")

(defun evergreen-grid-get-face (status)
  (cond
   ((string= "success" status) 'evergreen-grid-success)
   ((string= "failed" status) 'evergreen-grid-failed)
   ((string= "started" status) 'evergreen-grid-started)
   ((string= "undispatched" status) 'evergreen-grid-undispatched)
   ((string= "system-failed" status) 'evergreen-grid-system-failure)
   (t 'evergreen-grid-undispatched)
   ))

(defun evergreen-grid-create (title elements)
  (with-temp-buffer
    (seq-do
     (lambda (element)
       (insert
        (with-temp-buffer
          (insert " ")
          (add-text-properties
           (point-min)
           (point-max)
           (list 'face (evergreen-grid-get-face (evergreen-grid-element-status element))
                 'help-echo (evergreen-grid-element-description element)
                 'cursor-sensor-functions (list
                                           (lambda (w p action)
                                             (if (eq action 'entered)
                                                 (message (evergreen-grid-element-description element)))))
                 'evergreen-element-data (evergreen-grid-element-data element)))
          (insert
           (propertize
            " "
            'display '(space . (:width (1)))
            'cursor-intangible t
            'rear-nonsticky t
            'front-sticky t
            ))
          (buffer-string))))
     elements)
    (buffer-string)))
