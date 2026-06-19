;;; org-heading-at-point.el

(defun andy/org-study/get-flashcard-types-on-heading-at-point ()
  (let* ((text (org-get-heading 'no-todo 'no-tags))
         (tags (org-get-tags nil t))
         (types nil))
    (unless (or (cl-some (lambda (tag) (equal "edit-later" tag)) tags)
                (string-match-p ":edit-later:" text))
      (when (string-match-p SINGLE-DELIMITER text) (push 'SINGLE types))
      (when (string-match-p BI-DELIMITER text) (push 'BI types))
      (when (member TREECLOZE-TAG tags) (push 'TREECLOZE types))
      (when (string-match-p "\\*[^*]+\\*" text) (push 'CLOZE types)))
    types))

(provide 'org-heading-at-point)
