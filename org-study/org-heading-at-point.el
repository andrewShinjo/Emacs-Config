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
  (message "get-question-context-at-point")
  (save-excursion
    (let ((ctx nil))
      (when-let ((title (andy/org-study/get-title-at-point)))
        (push (format "#+title: %s" title) ctx)
        (push "" ctx))
      (while (org-up-heading-safe)
        (push (concat
               (make-string (org-outline-level) ?*)
               " "
               (org-get-heading 'no-todo 'no-tags)
               "\n"
               (andy/org-heading-at-point/get-body-text))
              ctx))
      (string-join (reverse ctx) "\n"))))

(defun andy/org-study/get-title-at-point ()
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward "^[ \t]*#\\+title:[ \t]*\\(.*\\)$" nil t)
        (string-trim (match-string 1))))))

(provide 'org-heading-at-point)
