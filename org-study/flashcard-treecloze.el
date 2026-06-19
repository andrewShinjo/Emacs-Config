;;; flashcard-treecloze.el

(defconst TREECLOZE-DUE-PROPERTY-PREFIX "TREECLOZE_DUE_")
(defconst TREECLOZE-INTERVAL-PROPERTY-PREFIX "TREECLOZE_INTERVAL_")
(defconst TREECLOZE-EASE-FACTOR-PROPERTY-PREFIX "TREECLOZE_EASE_FACTOR_")
(defconst TREECLOZE-REPETITION-PROPERTY-PREFIX "TREECLOZE_REPETITION_")
(defconst TREECLOZE-TAG "treecloze")

(defun andy/org-study/flashcard-treecloze/save ()
  (let ((suffix (number-to-string (plist-get flashcard :cloze-idx))))
    (org-entry-put (point) (concat TREECLOZE-DUE-PROPERTY-PREFIX suffix) due)
    (org-entry-put (point) (concat TREECLOZE-REPETITION-PROPERTY-PREFIX suffix) (number-to-string repetition))
    (org-entry-put (point) (concat TREECLOZE-EASE-FACTOR-PROPERTY-PREFIX suffix) (format "%.2f" ease-factor))
    (org-entry-put (point) (concat TREECLOZE-INTERVAL-PROPERTY-PREFIX suffix) (number-to-string interval))))

(defun andy/org-study/flashcard-treecloze/parse () nil)

(defun andy/org-study/flashcard-treecloze/properties ()
  "Returns a list of TREECLOZE property names."
  (list TREECLOZE-DUE-PROPERTY-PREFIX
	TREECLOZE-INTERVAL-PROPERTY-PREFIX
	TREECLOZE-EASE-FACTOR-PROPERTY-PREFIX
	TREECLOZE-REPETITION-PROPERTY-PREFIX))

(provide 'flashcard-treecloze)

