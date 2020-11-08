;;; -*- lexical-binding: t; -*-

(provide 'evergreen-grid)

(require 'cl-lib)

(cl-defstruct evergreen-grid-element description status)

(defface evergreen-grid-success
  '((t
     :background "forest green"
     :foreground "black"
     :box t))
  "success grid")

(defface evergreen-grid-fail
  '((t
     :background "red"
     :foreground "black"
     :box t))
  "fail grid")

(defface evergreen-grid-mouse
  '((t
     :background "yellow"
     :foreground "black"
     :box t))
  "fail grid")


(defun hover-success (window prev-pos action) 
  (message "in here %d" (point))
  (message "%s" action)
  (if (eq 'entered action)
      (progn
        (message "woooooo")
        (set-text-properties (point) (+ (point) 1) '('face 'evergreen-grid-mouse)))
    (set-text-properties (point) (+ (point) 1) (list 'face 'evergreen-grid-success))))

;; (defun hover-fail () 
;;   (lambda (window prev-pos action)
;;     (message "in here")
;;     (if (eq 'entered action)
;;         (add-text-properties (point) (point) '('face 'evergreen-grid-mouse))
;;       (add-text-properties (point) (point) (list 'face 'evergreen-grid-fail)))))

;; (defun hover (face) 
;;   (lambda (window prev-pos action)
;;     (message "in here")
;;     (if (eq 'entered action)
;;         (add-text-properties (point) (point) '('face 'evergreen-grid-mouse))
;;       (add-text-properties (point) (point) (list 'face face)))))

(defun evergreen-grid-create (title elements)
  (with-temp-buffer
    (seq-do
     (lambda (element)
       (insert
        (with-temp-buffer
          (insert " ")
          (let ((face
                 (if (string= "success" (evergreen-grid-element-status element))
                     'evergreen-grid-success
                   'evergreen-grid-fail)))
            (add-text-properties
             (point-min)
             (point-max)
             (list
              'face face
              'mouse-face 'evergreen-grid-mouse
              'cursor-sensor-functions (list 'hover-success)
              )))
          ;; (buffer-face-set 'evergreen-grid-fail)
          ;; (buffer-face-mode)
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
