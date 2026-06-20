;;; org-heading-at-point.el

(defun andy/org-heading-at-point/get-body-text ()
  (save-excursion
    (org-back-to-heading t) (org-end-of-meta-data t)
    (let ((lines '()))
      (while (and (not (eobp)) (not (looking-at "^\\*+ ")))
        (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) lines)
        (forward-line 1))
      (string-trim (mapconcat #'identity (nreverse lines) "\n")))))

(defun andy/org-study/get-flashcard-types-on-heading-at-point ()
  (let* ((text (org-get-heading 'no-todo 'no-tags))
         (tags (org-get-tags nil t))
         (types nil))
    (pcase-dolist (`(,type . ,props) org-study--flashcard-handlers)
      (let ((is-flashcard-fn (cdr (assoc :is-flashcard props))))
        (when (and is-flashcard-fn (funcall is-flashcard-fn text tags))
          (push type types))))
    types))

(defun andy/org-study/get-question-context-at-point ()
  (save-excursion
    (let (ctx)
      (while (org-up-heading-safe)
        (push (concat (make-string (org-outline-level) ?*) " " (org-get-heading 'no-todo 'no-tags) "\n\n" (andy/org-heading-at-point/get-body-text)) ctx))
      (mapconcat #'identity ctx "\n"))))

(provide 'org-heading-at-point)
