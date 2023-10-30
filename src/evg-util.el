;;; -*- lexical-binding: t; -*-

(provide 'evg-util)

(defun evg--gethash (hash &rest keys)
  (if (eq keys nil)
      nil
    (let* ((key (pop keys))
           (value (gethash key hash)))
      (if (eq keys nil)
          value
        (apply 'evg--gethash value keys)))))

(defun evg--try-forward-char ()
  "Attempt to move the point forward one char.

If the end of the buffer is hit, returns nil."
  (condition-case nil (progn (forward-char) t) (error nil)))

(defun evg--try-backward-char ()
  "Attempt to move the point backward one char.

If the beginning of the buffer is hit, returns nil."
  (condition-case nil (progn (backward-char) t) (error nil)))

(defun evg--advance-until (travel-fn condition-fn)
  "Advance point until the condition is met. If the point reaches the end of the buffer before the condition is met,
the point is reset to its initial place. Returns whether the condition was met."
  (let ((initial-point (point))
        (condition))
    (while (and
            (funcall travel-fn)
            (not (setq condition (funcall condition-fn)))))
    (when (not condition)
      (goto-char initial-point))
    condition))
