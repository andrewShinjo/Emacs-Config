;;; flashcard-cloze.el

(defconst CLOZE-REGEX "\\*[^*]+\\*")

(defconst CLOZE-DUE-PROPERTY-PREFIX "CLOZE_DUE_")
(defconst CLOZE-INTERVAL-PROPERTY-PREFIX "CLOZE_INTERVAL_")
(defconst CLOZE-EASE-FACTOR-PROPERTY-PREFIX "CLOZE_EASE_FACTOR_")
(defconst CLOZE-REPETITION-PROPERTY-PREFIX "CLOZE_REPETITION_")

(defun andy/org-study/flashcard-cloze/is-flashcard (text tags)
  (string-match-p CLOZE-REGEX text))

(defun andy/org-study/flashcard-cloze/save (flashcard)
  (let ((suffix (number-to-string (plist-get flashcard :cloze-idx)))
        (due (plist-get flashcard :due))
        (repetition (plist-get flashcard :repetition))
        (ease-factor (plist-get flashcard :ease-factor))
        (interval (plist-get flashcard :interval)))
    (org-entry-put (point) (concat CLOZE-DUE-PROPERTY-PREFIX suffix) due)
    (org-entry-put (point) (concat CLOZE-REPETITION-PROPERTY-PREFIX suffix) (number-to-string repetition))
    (org-entry-put (point) (concat CLOZE-EASE-FACTOR-PROPERTY-PREFIX suffix) (format "%.2f" ease-factor))
    (org-entry-put (point) (concat CLOZE-INTERVAL-PROPERTY-PREFIX suffix) (number-to-string interval))))

(defun andy/org-study/flashcard-cloze/parse (org-file now)
  (let* ((text (org-get-heading 'no-todo 'no-tags))
         (context (andy/org-study/get-question-context-at-point))
         (level (org-outline-level))
         (ID (org-entry-get nil "ID"))
         (answers nil)
         (start 0)
         (due-cards nil))
    
    (while (string-match CLOZE-REGEX text start)
      (push (match-string 1 text) answers)
      (setq start (match-end 0)))
    (setq answers (nreverse answers))
    
    (dotimes (i (length answers))
      (let* ((suffix (number-to-string i))
             (due (org-entry-get nil (concat CLOZE-DUE-PROPERTY-PREFIX suffix)))
             (is-due (or (not due) (time-less-p (org-time-string-to-time due) now))))
        (when is-due
          (push (list
                 :org-file org-file
                 :ID ID
                 :question (concat
			    context
			    "\n"
			    (make-string level ?*)
			    " "
			    (andy/org-study/make-cloze-question text i))
                 :answer (nth i answers)
                 :due (or due "")
                 :repetition (string-to-number
			      (or (org-entry-get nil (concat CLOZE-REPETITION-PROPERTY-PREFIX suffix)) "0"))
                 :ease-factor (string-to-number
			       (or (org-entry-get nil (concat CLOZE-EASE-FACTOR-PROPERTY-PREFIX suffix)) "2.5"))
                 :interval (string-to-number
			    (or (org-entry-get nil (concat CLOZE-INTERVAL-PROPERTY-PREFIX suffix)) "0"))
                 :type 'CLOZE
                 :cloze-idx i)
                due-cards))))    
    (nreverse due-cards)))

(defun andy/org-study/flashcard-cloze/properties ()
  "Returns a list of CLOZE property names."
  (list CLOZE-DUE-PROPERTY-PREFIX
	CLOZE-INTERVAL-PROPERTY-PREFIX
	CLOZE-EASE-FACTOR-PROPERTY-PREFIX
	CLOZE-REPETITION-PROPERTY-PREFIX))

(defun andy/org-study/make-cloze-question (text target-idx)
  (let ((idx 0) (result text))
    (while (string-match "\\*\\([^*]+\\)\\*" result)
      (if (= idx target-idx) (setq result (replace-match "[...]" t t result))
        (setq result (replace-match (match-string 1 result) t t result)))
      (setq idx (1+ idx)))
    result))

(provide 'flashcard-cloze)
