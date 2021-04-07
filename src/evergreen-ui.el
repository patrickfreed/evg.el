(provide 'evergreen-ui)

(require 'seq)

;; from section-header in magit
(defface evergreen-header
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey95")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey20"))
  "Face for the header portion of an Evergreen buffer"
  :group 'evergreen)

(defface evergreen-header-title
  '((t
     :bold t
     :height 1.25
     :underline t
     :rear-nonsticky t))
  "Face for the title of the header of an Evergreen buffer"
  :group 'evergreen)

(defun evergreen-ui-insert-header (items &optional title)
  (let ((start (point)))
    (when title
      (insert (propertize title 'face 'evergreen-header-title))
      (insert " ")
      (newline 2))
    (seq-do
     (lambda (pair)
       (insert
        (format "%-16s"
                (with-temp-buffer
                  (insert (format "%s:" (car pair)))
                  (add-text-properties (point-min) (point-max) (list 'face 'bold))
                  (buffer-string)))
        (with-temp-buffer
          (setq fill-column (- (window-width) 26))
          (setq fill-prefix (make-string 16 ? ))
          (insert (cdr pair))
          (fill-paragraph)
          (buffer-string))
        )
       (newline))
     items)
    (let ((overlay (make-overlay start (point))))
      (overlay-put overlay 'face 'evergreen-header))))
  
