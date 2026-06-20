;;; flashcard-treecloze.el

(defconst TREECLOZE-DUE-PROPERTY-PREFIX "TREECLOZE_DUE_")
(defconst TREECLOZE-INTERVAL-PROPERTY-PREFIX "TREECLOZE_INTERVAL_")
(defconst TREECLOZE-EASE-FACTOR-PROPERTY-PREFIX "TREECLOZE_EASE_FACTOR_")
(defconst TREECLOZE-REPETITION-PROPERTY-PREFIX "TREECLOZE_REPETITION_")
(defconst TREECLOZE-TAG "treecloze")

(defun andy/org-study/flashcard-treecloze/is-flashcard (text tags)
  (member TREECLOZE-TAG tags))

(defun andy/org-study/flashcard-treecloze/save (flashcard)
  (let ((suffix (number-to-string (plist-get flashcard :cloze-idx)))
        (due (plist-get flashcard :due))
        (repetition (plist-get flashcard :repetition))
        (ease-factor (plist-get flashcard :ease-factor))
        (interval (plist-get flashcard :interval)))
    (org-entry-put (point) (concat TREECLOZE-DUE-PROPERTY-PREFIX suffix) due)
    (org-entry-put (point) (concat TREECLOZE-REPETITION-PROPERTY-PREFIX suffix) (number-to-string repetition))
    (org-entry-put (point) (concat TREECLOZE-EASE-FACTOR-PROPERTY-PREFIX suffix) (format "%.2f" ease-factor))
    (org-entry-put (point) (concat TREECLOZE-INTERVAL-PROPERTY-PREFIX suffix) (number-to-string interval))))

(defun andy/org-study/flashcard-treecloze/parse (org-file now)
  (let* ((level (org-current-level))
         (context (andy/org-study/get-question-context-at-point))
         (parent (org-get-heading 'no-todo 'no-tags))
         (id (org-entry-get nil "ID"))
         (children (andy/org-study/flashcard-treecloze/children-data))
         (n (length children))
         cards)
    (dotimes (i n)
      (let* ((suffix (number-to-string i))
             (due (org-entry-get nil (concat TREECLOZE-DUE-PROPERTY-PREFIX suffix)))
             (is-due (or (not due) (time-less-p (org-time-string-to-time due) now))))
        (when is-due
          (let* ((target (nth i children))
                 (q-lines
                  (cl-loop for child in children
                           for j from 0
                           collect
                           (let ((is-target (= i j)))
                             (format "%s %s%s"
                                     (make-string (1+ level) ?*)
                                     (if is-target "[...]" (plist-get child :title))
                                     (if is-target
                                         ""
                                       (let ((b (plist-get child :body)))
                                         (if (string-empty-p b)
                                             ""
                                           (concat "\n" b))))))))
                 (question (concat context
                                   "\n"
                                   (make-string level ?*) " " parent
                                   "\n"
                                   (mapconcat #'identity q-lines "\n")))
                 (answer (concat (plist-get target :title)
                                 "\n\n"
                                 (plist-get target :body))))
            (push (list
		   :org-file org-file
                   :ID id
                   :question question
                   :answer answer
                   :due (or due "")
                   :repetition (string-to-number
                                (or (org-entry-get nil (concat TREECLOZE-REPETITION-PROPERTY-PREFIX suffix))
                                    "0"))
                   :ease-factor (string-to-number
                                 (or (org-entry-get nil (concat TREECLOZE-EASE-FACTOR-PROPERTY-PREFIX suffix))
				     "2.5"))
                   :interval (string-to-number
                              (or (org-entry-get nil (concat TREECLOZE-INTERVAL-PROPERTY-PREFIX suffix)) "0"))
                   :type 'TREECLOZE
                   :cloze-idx i)
                  cards)))))
    (nreverse cards)))

(defun andy/org-study/flashcard-treecloze/properties ()
  "Returns a list of TREECLOZE property names."
  (list TREECLOZE-DUE-PROPERTY-PREFIX
	TREECLOZE-INTERVAL-PROPERTY-PREFIX
	TREECLOZE-EASE-FACTOR-PROPERTY-PREFIX
	TREECLOZE-REPETITION-PROPERTY-PREFIX))

(defun andy/org-study/flashcard-treecloze/children-data ()
  (save-excursion
    (when (org-goto-first-child)
      (let (result)
	(push (list
	       :title (org-get-heading 'no-todo 'no-tags)
	       :body (andy/org-heading-at-point/get-body-text))
	      result)
	(while (org-get-next-sibling)
	  (push (list
		 :title (org-get-heading 'no-todo 'no-tags)
		 :body (andy/org-heading-at-point/get-body-text))
		result))
	(nreverse result)))))


(provide 'flashcard-treecloze)

