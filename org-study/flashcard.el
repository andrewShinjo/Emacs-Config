;;; flashcard.el

(require 'flashcard-bi)
(require 'flashcard-cloze)
(require 'flashcard-single)
(require 'flashcard-treecloze)

(defun org-study--get-handler (type operation)
  "Get handler function for TYPE and OPERATION (:save, :parse, :interval)."
  (alist-get operation (alist-get type org-study--flashcard-handlers)))

(defvar org-study--flashcard-handlers
  `(
    (SINGLE . ((:is-flashcard . andy/org-study/flashcard-single/is-flashcard)
	       (:save . andy/org-study/flashcard-single/save)
	       (:parse . andy/org-study/flashcard-single/parse)
	       (:props . andy/org-study/flashcard-single/properties)))

    (BI . ((:is-flashcard . andy/org-study/flashcard-bi/is-flashcard)
	   (:save . andy/org-study/flashcard-bi/save)
	   (:parse . andy/org-study/flashcard-bi/parse)
	   (:props . andy/org-study/flashcard-bi/properties)))

    (CLOZE . ((:is-flashcard . andy/org-study/flashcard-cloze/is-flashcard)	      
	      (:save . andy/org-study/flashcard-cloze/save)
	      (:parse . andy/org-study/flashcard-cloze/parse)
	      (:props . andy/org-study/flashcard-cloze/properties)))

    (TREECLOZE . ((:is-flashcard . andy/org-study/flashcard-treecloze/is-flashcard)
		  (:save . andy/org-study/flashcard-treecloze/save)
		  (:parse . andy/org-study/flashcard-treecloze/parse)
		  (:props . andy/org-study/flashcard-treecloze/properties)))))


(defun andy/org-study/flashcard/save ()
  (let* ((flashcard (car due-flashcards))
         (id (plist-get flashcard :ID))
         (due (plist-get flashcard :due))
         (repetition (plist-get flashcard :repetition))
         (ease-factor (plist-get flashcard :ease-factor))
         (interval (plist-get flashcard :interval))
         (flashcard-type (plist-get flashcard :type))
         (marker (org-id-find id 'marker))
	 (save-handler (org-study--get-handler flashcard-type :save)))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char (marker-position marker))
          (funcall save-handler flashcard)
          (save-buffer))))))

(provide 'flashcard)
