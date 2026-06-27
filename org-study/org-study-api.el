;;; org-study-api.el

(require 'org-study-model)
(require 'org-element)
(require 'seq)
(require 'subr-x)
(require 'file-api)
(require 'org-study)
(require 'vtable)

(defconst BUFFER-NAME "*ORG-STUDY*")
(defconst ID-PROPERTY "ID")
(defconst ORG-FILE-REGEX "\\`[^.#].*\\.org\\'")
(defconst REVIEW-DUE-PROPERTY "REVIEW_DUE")
(defconst REVIEW-INCREMENT-PROPERTY "REVIEW_INCREMENT")

(defconst REVIEW-TAGS '(
			"article"
			"edit-later"
			"extract"))

(defalias 'org-study/start-study 'andy/org-study/start-study)

(defun org-study--rate-review-note (object interested-p)
  (let* ((heading-id (heading-id object))
         (current-due (heading-review-due object))
         (current-inc (heading-review-increment object))
         (current-due-time (date-to-time current-due))
         (inc-days (days-to-time (string-to-number current-inc)))
         (base-time (if (time-less-p current-due-time (current-time))
                        (current-time)
                      current-due-time))
         (next-due (format-time-string "%Y-%m-%d %H:%M"
                                       (time-add base-time inc-days)))
         (next-inc (number-to-string
                    (if interested-p
                        (max 1 (1- (string-to-number current-inc)))
                      (max 4 (1+ (string-to-number current-inc))))))
         (label (if interested-p "Interested" "Not interested")))
    (save-window-excursion
      (org-id-goto heading-id)
      (org-entry-put (point) REVIEW-DUE-PROPERTY next-due)
      (org-entry-put (point) REVIEW-INCREMENT-PROPERTY next-inc))
    (vtable-remove-object (vtable-current-table) object)
    (message "%s. Next review due: %s, Next incremental: %s"
             label next-due next-inc)))

(defun andy/org-study/review-notes ()
  
  (interactive)

  (let* ((all-files (andy/file-api/get-files
		     org-directory
		     :file-extensions '("org")
		     :recursive t))
         (all-headings
          (cl-mapcan
           (lambda (org-file)
             (with-current-buffer (find-file-noselect org-file)
               (org-map-entries
                (lambda ()
		  (let ((p (point))
			(id (org-id-get (point) 'create))
			(text (org-get-heading 'no-todo 'no-tags))
			(review-due (or (org-entry-get (point) REVIEW-DUE-PROPERTY) (format-time-string "%Y-%m-%d")))
			(review-increment (or (org-entry-get (point) REVIEW-INCREMENT-PROPERTY "4")))
			(tags (org-get-tags nil t)))
                  (make-heading
                   :file org-file
                   :id id
		   :text text
                   :review-due review-due
		   :review-increment review-increment
                   :tags tags))
		t 'file (string-join REVIEW-TAGS "|"))))
           all-files)))

         (headings-filtered-by-due
          (cl-remove-if-not
           (lambda (heading)
             (let ((review-due (heading-review-due heading))
                   (now (current-time)))
               (time-less-p (org-time-string-to-time review-due) now)))
           all-headings))

	 (sorted
	  (sort headings-filtered-by-due
	  (lambda (a b)
	    (string< (heading-review-due a) (heading-review-due b))))))
    
    ;; function here
    
    (let ((buffer (get-buffer-create BUFFER-NAME)))
      (with-current-buffer buffer
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (insert "Review Queue:\n")
	  (make-vtable
	   :columns '(
		      (:name "Text" :width 50)
		      (:name "Tags" :width 16)
		      (:name "Review Due" :width 50))
	   :objects sorted
	   :getter (lambda (object column vtable)
		     (pcase (vtable-column vtable column)
		       ("Text" (heading-text object))
		       ("Tags" (mapconcat #'identity (heading-tags object) ", "))
		       ("Review Due" (heading-review-due object))))
	   :actions '(
		      "RET"
		      (lambda (object) (org-id-goto (heading-id object)))
		      
		      "i"
		      (lambda (object) (org-study--rate-review-note object t))
		      "n"
		      (lambda (object) (org-study--rate-review-note object nil)))
	   :separator-width 3)
	  (setq buffer-read-only t)
	  (switch-to-buffer buffer))))))

(provide 'org-study-api)
