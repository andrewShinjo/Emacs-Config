;;; flashcard-cloze.el

(defconst CLOZE-DUE-PROPERTY-PREFIX "CLOZE_DUE_")
(defconst CLOZE-INTERVAL-PROPERTY-PREFIX "CLOZE_INTERVAL_")
(defconst CLOZE-EASE-FACTOR-PROPERTY-PREFIX "CLOZE_EASE_FACTOR_")
(defconst CLOZE-REPETITION-PROPERTY-PREFIX "CLOZE_REPETITION_")

(defun andy/org-study/flashcard-cloze/save ()
  (let ((suffix (number-to-string (plist-get flashcard :cloze-idx))))
    (org-entry-put (point) (concat CLOZE-DUE-PROPERTY-PREFIX suffix) due)
    (org-entry-put (point) (concat CLOZE-REPETITION-PROPERTY-PREFIX suffix) (number-to-string repetition))
    (org-entry-put (point) (concat CLOZE-EASE-FACTOR-PROPERTY-PREFIX suffix) (format "%.2f" ease-factor))
    (org-entry-put (point) (concat CLOZE-INTERVAL-PROPERTY-PREFIX suffix) (number-to-string interval))))

(defun andy/org-study/flashcard-cloze/parse () nil)

(defun andy/org-study/flashcard-cloze/properties ()
  "Returns a list of CLOZE property names."
  (list CLOZE-DUE-PROPERTY-PREFIX
	CLOZE-INTERVAL-PROPERTY-PREFIX
	CLOZE-EASE-FACTOR-PROPERTY-PREFIX
	CLOZE-REPETITION-PROPERTY-PREFIX))

(provide 'flashcard-cloze)
