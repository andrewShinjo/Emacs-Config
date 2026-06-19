;;; org-heading-at-point.el

(require 'flashcard)
(require 'flashcard-bi)
(require 'flashcard-single)
(require 'flashcard-treecloze)

(defun andy/org-study/get-flashcard-types-on-heading-at-point ()
  (let* ((text (org-get-heading 'no-todo 'no-tags))
         (tags (org-get-tags nil t))
         (types nil))
    (pcase-dolist (`(,type . ,props) org-study--flashcard-handlers)
      (let ((is-flashcard-fn (cdr (assoc :is-flashcard props))))
	(when is-flashcard-fn
	  (when (funcall is-flashcard-fn text tags)
	    (push type types)))))
    types))

(provide 'org-heading-at-point)
