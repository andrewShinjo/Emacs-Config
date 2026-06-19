;;; flashcard-single.el

(require 'org-heading-at-point)

(defconst SINGLE-DELIMITER " :-> ")
(defconst SINGLE-TAG "single")

(defconst SINGLE-DUE-PROPERTY "SINGLE_DUE")
(defconst SINGLE-INTERVAL-PROPERTY "SINGLE_INTERVAL")
(defconst SINGLE-EASE-FACTOR-PROPERTY "SINGLE_EASE_FACTOR")
(defconst SINGLE-REPETITION-PROPERTY "SINGLE_REPETITION")

(defun andy/org-study/flashcard-single/is-flashcard(text tags)
  (member SINGLE-TAG tags))

(defun andy/org-study/flashcard-single/save (flashcard)
  (let ((due (plist-get flashcard :due))
	(repetition (plist-get flashcard :repetition))
	(ease-factor (plist-get flashcard :ease-factor))
	(interval (plist-get flashcard :interval)))
    (org-entry-put (point) SINGLE-DUE-PROPERTY due)
    (org-entry-put (point) SINGLE-REPETITION-PROPERTY (number-to-string repetition))
    (org-entry-put (point) SINGLE-EASE-FACTOR-PROPERTY (format "%.2f" ease-factor))
    (org-entry-put (point) SINGLE-INTERVAL-PROPERTY (number-to-string interval))))

(defun andy/org-study/flashcard-single/parse (org-file now)
  (let* (
	 (heading-text (org-get-heading 'no-todo 'no-tags))
	 (body-text (andy/org-heading-at-point/get-body-text))
	 (ID (org-entry-get nil "ID"))
	 (repetition (string-to-number (or (org-entry-get nil SINGLE-REPETITION-PROPERTY) "0")))
	 (ease-factor (string-to-number (or (org-entry-get nil SINGLE-EASE-FACTOR-PROPERTY) "2.5")))
	 (interval (string-to-number (or (org-entry-get nil SINGLE-INTERVAL-PROPERTY) "0")))
	 (due (org-entry-get nil SINGLE-DUE-PROPERTY))
	 (is-due (or (not due) (time-less-p (org-time-string-to-time due) now))))
    (if is-due
	(list
	 :org-file org-file
	 :ID ID
	 :question heading-text
	 :answer body-text
	 :repetition repetition
	 :ease-factor ease-factor
	 :interval interval
	 :type 'SINGLE)
      nil)))

(defun andy/org-study/flashcard-single/properties ()
  "Returns a list of SINGLE property names."
  (list SINGLE-DUE-PROPERTY
	SINGLE-INTERVAL-PROPERTY
	SINGLE-EASE-FACTOR-PROPERTY
	SINGLE-REPETITION-PROPERTY))

(provide 'flashcard-single)
