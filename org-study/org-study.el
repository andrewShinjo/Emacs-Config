;;; org-study.el --- Simple SRS and Curation Queue for Org Mode

(require 'cl-lib)
(require 'flashcard)
(require 'flashcard-single)
(require 'org-element)
(require 'org-heading-at-point)

;; --- Constants for SM2 Properties ---
(defconst HASH-PROPERTY "HASH")
(defconst TREECLOZE-HASH-PROPERTY "TREE_CLOZE_HASH")

;; Flashcard Properties

(defvar due-flashcards nil "Current list of due cards in a study session.")
(defvar randomized-queue nil "Current list of markers for the processing queue.")

;;; --- UI & Mode ---

(define-derived-mode flashcard-mode org-mode "FlashcardMode"
  "Major mode for reviewing flashcards."
  (read-only-mode 1)
  (setq-local cursor-type nil)
  (keymap-set flashcard-mode-map "SPC" 'andy/org-study/show-answer)
  (keymap-set flashcard-mode-map "e" 'andy/org-study/rate-flashcard-easy)
  (keymap-set flashcard-mode-map "h" 'andy/org-study/rate-flashcard-hard)
  (keymap-set flashcard-mode-map "f" 'andy/org-study/rate-flashcard-forgot)
  (keymap-set flashcard-mode-map "E" 'andy/org-study/mark-edit-later))

(defun andy/org-study/mark-edit-later ()
  "Flag the current card with :edit-later: and skip it."
  (interactive)
  (let* ((flashcard (car due-flashcards))
         (id (plist-get flashcard :ID))
         (marker (org-id-find id 'marker)))
    (if (not marker)
        (message "Error: ID not found.")
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char (marker-position marker))
          (org-set-tags (append (org-get-tags nil t) '("edit-later"))))
        (save-buffer)
        (message "Card marked :edit-later: and removed from current session."))
      (pop due-flashcards)
      (andy/org-study/display-flashcard-question))))

(defun andy/org-study/rate-flashcard-easy ()
  (interactive)
  (let ((flashcard (car due-flashcards)))
    (setcar due-flashcards (andy/org-study/update-card-sm2 flashcard 5))
    (andy/org-study/flashcard/save)
    (pop due-flashcards)
    (andy/org-study/display-flashcard-question)))

(defun andy/org-study/rate-flashcard-hard ()
  (interactive)
  (let ((flashcard (car due-flashcards)))
    (setcar due-flashcards (andy/org-study/update-card-sm2 flashcard 3))
    (andy/org-study/flashcard/save)
    (pop due-flashcards)
    (andy/org-study/display-flashcard-question)))

(defun andy/org-study/rate-flashcard-forgot ()
  (interactive)
  (let ((flashcard (car due-flashcards)))
    (setcar due-flashcards (andy/org-study/update-card-sm2 flashcard 1))
    (andy/org-study/flashcard/save)
    (pop due-flashcards)
    (andy/org-study/display-flashcard-question)))

;;; --- Core SRS Logic ---

(defun andy/org-study/shuffle-list (list)
  (let ((vec (vconcat list))
        (len (length list)))
    (dotimes (i len)
      (let ((j (+ i (random (- len i))))
            (tmp (aref vec i)))
        (aset vec i (aref vec j))
        (aset vec j tmp)))
    (append vec nil)))

(defun andy/org-study/update-card-sm2 (flashcard quality)
  (let ((repetition (plist-get flashcard :repetition))
        (ease-factor (plist-get flashcard :ease-factor))
        (interval (plist-get flashcard :interval)))
    (if (>= quality 3)
        (cond ((= repetition 0) (setq interval 1))
              ((= repetition 1) (setq interval 6))
              (t (setq interval (round (* interval ease-factor)))))
      (setq repetition 0)
      (setq interval 1))
    (setq ease-factor (+ ease-factor (- 0.1 (* (- 5 quality) (+ 0.08 (* (- 5 quality) 0.02))))))
    (when (< ease-factor 1.3) (setq ease-factor 1.3))
    (let ((new-card (copy-sequence flashcard)))
      (plist-put new-card :repetition (if (>= quality 3) (1+ repetition) 0))
      (plist-put new-card :ease-factor ease-factor)
      (plist-put new-card :interval interval)
      (plist-put new-card :due (format-time-string "%Y-%m-%d %H:%M" (time-add (current-time)
									      (days-to-time interval))))
      new-card)))

(defun andy/org-study/get-flashcards-in-org-file (org-file)
  (with-current-buffer (find-file-noselect org-file)
    (let ((all-flashcards '())
          (now (current-time)))
      (org-map-entries
       (lambda ()
         (let ((types (andy/org-study/get-flashcard-types-on-heading-at-point)))
           (dolist (type types)
             (when-let ((parse-fn (org-study--get-handler type :parse)))
               (let ((result (funcall parse-fn org-file now)))
                 (when result
                   (if (keywordp (car result))
                       (push result all-flashcards)
                     (setq all-flashcards
                           (nconc (cl-copy-list result) all-flashcards)))))))))
       nil 'file)
      (nreverse all-flashcards))))

(defun andy/org-study/delete-properties ()
  (let ((types (andy/org-study/get-flashcard-types-on-heading-at-point)))
    (when types
      (let* ((text (org-get-heading 'no-todo 'no-tags))
             (new-hash (secure-hash 'sha256 text))
             (old-hash (org-entry-get nil HASH-PROPERTY))
             (new-tree-hash (secure-hash 'sha256 (andy/org-study/serialize-child-headings)))
             (old-tree-hash (org-entry-get nil TREECLOZE-HASH-PROPERTY)))
        (when (not (equal new-hash old-hash))
          (dolist (p (list SINGLE-DUE-PROPERTY SINGLE-INTERVAL-PROPERTY SINGLE-EASE-FACTOR-PROPERTY SINGLE-REPETITION-PROPERTY
                           BI-DUE-FORWARD-PROPERTY BI-INTERVAL-FORWARD-PROPERTY BI-EASE-FACTOR-FORWARD-PROPERTY BI-REPETITION-FORWARD-PROPERTY
                           BI-DUE-REVERSE-PROPERTY BI-INTERVAL-REVERSE-PROPERTY BI-EASE-FACTOR-REVERSE-PROPERTY BI-REPETITION-REVERSE-PROPERTY))
            (org-entry-delete nil p))
          (let ((props (org-entry-properties nil 'standard)))
            (dolist (entry props) (when (string-match-p "CLOZE_\\(DUE\\|INTERVAL\\|EASE_FACTOR\\|REPETITION\\)_" (car entry)) (org-entry-delete nil (car entry)))))
          (org-entry-put nil HASH-PROPERTY new-hash))
        (when (not (equal new-tree-hash old-tree-hash))
          (let ((props (org-entry-properties nil 'standard)))
            (dolist (entry props) (when (string-match-p "TREECLOZE_\\(DUE\\|INTERVAL\\|EASE_FACTOR\\|REPETITION\\)_" (car entry)) (org-entry-delete nil (car entry)))))
          (org-entry-put nil TREECLOZE-HASH-PROPERTY new-tree-hash))))))

(defun andy/org-study/create-properties () 
  "Only create an ID if the heading is actually a flashcard."
  (when (andy/org-study/get-flashcard-types-on-heading-at-point) 
    (org-id-get-create)))

(defun andy/org-study/serialize-child-headings ()
  (let (children)
    (save-excursion
      (when (org-goto-first-child)
	(push (org-get-heading 'no-todo 'no-tags) children) (while (org-get-next-sibling) (push (org-get-heading 'no-todo 'no-tags) children)))) (mapconcat #'identity (sort children #'string<) " | ")))


(defun andy/org-study/show-answer ()
  "Reveals the answer in the flashcard buffer, bypassing read-only mode."
  (interactive)
  (with-current-buffer "FlashcardMode"
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert "\n\n" (make-string 30 ?-) "\n" "**Answer:**\n" 
                (or (plist-get (car due-flashcards) :answer) "No answer found."))
        (insert "\n\n" "Easy [e], Hard [h], Forgot [f] | Mark Edit-Later [E]")))))

(defun andy/org-study/display-flashcard-question ()
  "Displays the next question, ensuring the buffer is cleared correctly."
  (let ((flashcard (car due-flashcards))
        (buf (get-buffer-create "FlashcardMode")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (flashcard-mode)
        (if (not flashcard)
            (insert "Done with flashcards.")
          (insert (format "Flashcards remaining: %d\n\n" (length due-flashcards)))
          (insert (plist-get flashcard :question))
          (insert "\n\n" "Show answer: [SPC]"))))
    (switch-to-buffer buf)))

(defun andy/org-study/start-study ()
  (interactive)
  (let ((files (andy/org-file/get-all-org-files-from-directory-recursively org-directory)))
    (dolist (f files) 
      (with-current-buffer (find-file-noselect f) 
        (org-map-entries 
         (lambda () 
           ;; Check for flashcard types BEFORE creating properties or IDs
           (when (andy/org-study/get-flashcard-types-on-heading-at-point)
             (andy/org-study/create-properties) 
             (andy/org-study/delete-properties))) 
         nil 'file) 
        (save-buffer))))
  (org-id-update-id-locations)
  (let ((files (andy/org-file/get-all-org-files-from-directory-recursively org-directory)) (collected '()))
    (dolist (f files) (setq collected (append collected (andy/org-study/get-flashcards-in-org-file f))))
    (setq due-flashcards (andy/org-study/shuffle-list collected))
    (andy/org-study/display-flashcard-question)))

;; --- Helper Functions ---

(defun andy/org-file/get-all-org-files-from-directory-recursively (dir)
  (let ((files (directory-files-recursively dir "\\.org$")))
    (cl-remove-if (lambda (f) (string-match-p "/\\.#\\|/#\\|~$\\|^\\." (file-name-nondirectory f))) files)))


(provide 'org-study)
