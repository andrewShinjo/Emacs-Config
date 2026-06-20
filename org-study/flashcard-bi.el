;;; flashcard-bi.el

(require 'org-heading-at-point)

(defconst BI-DELIMITER " :<-> ")
(defconst BI-TAG "bi")

(defconst BI-DUE-FORWARD-PROPERTY "BI_FORWARD_DUE")
(defconst BI-INTERVAL-FORWARD-PROPERTY "BI_INTERVAL_FORWARD")
(defconst BI-EASE-FACTOR-FORWARD-PROPERTY "BI_EASE_FACTOR_FORWARD")
(defconst BI-REPETITION-FORWARD-PROPERTY "BI_REPETITION_FORWARD")
(defconst BI-DUE-REVERSE-PROPERTY "BI_REVERSE_DUE")
(defconst BI-INTERVAL-REVERSE-PROPERTY "BI_INTERVAL_REVERSE")
(defconst BI-EASE-FACTOR-REVERSE-PROPERTY "BI_EASE_FACTOR_REVERSE")
(defconst BI-REPETITION-REVERSE-PROPERTY "BI_REPETITION_REVERSE")

(defun andy/org-study/flashcard-bi/is-flashcard (text tags)
  (member BI-TAG tags))

(defun andy/org-study/flashcard-bi/save (flashcard) nil)

(defun andy/org-study/flashcard-bi/parse (org-file now)

  (let* (
	 (heading-text (org-get-heading 'no-todo 'no-tags))
	 (body-text (andy/org-heading-at-point/get-body-text))
	 (ID (org-entry-get nil "ID"))
	 (forward-repetition (string-to-number (or (org-entry-get nil BI-REPETITION-FORWARD-PROPERTY) "0")))
	 (forward-ease-factor (string-to-number (or (org-entry-get nil BI-EASE-FACTOR-FORWARD-PROPERTY) "2.5")))
	 (forward-interval (string-to-number (or (org-entry-get nil BI-INTERVAL-FORWARD-PROPERTY) "0")))
	 (forward-due (org-entry-get nil BI-DUE-FORWARD-PROPERTY))
	 (is-forward-due (or (not forward-due) (time-less-p (org-time-string-to-time forward-due) now)))
	 (reverse-repetition (string-to-number (or (org-entry-get nil BI-REPETITION-REVERSE-PROPERTY) "0")))
	 (reverse-ease-factor (string-to-number (or (org-entry-get nil BI-EASE-FACTOR-REVERSE-PROPERTY) "2.5")))
	 (reverse-interval (string-to-number (or (org-entry-get nil BI-INTERVAL-REVERSE-PROPERTY) "0")))
	 (reverse-due (org-entry-get nil BI-DUE-REVERSE-PROPERTY))
	 (is-reverse-due (or (not reverse-due) (time-less-p (org-time-string-to-time reverse-due) now))))

    (let ((forward-card
	   (when is-forward-due
	     (list
	      :org-file org-file
	      :ID ID
	      :question heading-text
	      :answer body-text
	      :repetition forward-repetition
	      :ease-factor forward-ease-factor
	      :interval forward-interval
	      :type 'BI
	      :bi-type 'FORWARD)))
	  (reverse-card
	   (when is-reverse-due
	     (list
	      :org-file org-file
	      :ID ID
	      :question body-text
	      :answer heading-text
	      :repetition reverse-repetition
	      :ease-factor reverse-ease-factor
	      :interval reverse-interval
	      :type 'BI
	      :bi-type 'REVERSE))))
      (delq nil (list forward-card reverse-card)))))

(defun andy/org-study/flashcard-bi/properties ()
  "Returns a list of BI property names."
  (list BI-DUE-FORWARD-PROPERTY
	BI-INTERVAL-FORWARD-PROPERTY
	BI-EASE-FACTOR-FORWARD-PROPERTY
	BI-REPETITION-FORWARD-PROPERTY
	BI-DUE-REVERSE-PROPERTY
	BI-INTERVAL-REVERSE-PROPERTY
	BI-EASE-FACTOR-REVERSE-PROPERTY
	BI-REPETITION-REVERSE-PROPERTY))

(provide 'flashcard-bi)
