;;; org-study-api.el

(require 'org-study-model)
(require 'org-element)
(require 'seq)
(require 'subr-x)
(require 'vtable)
(require 'org-study)

(defconst BUFFER-NAME "*ORG-STUDY*")
(defconst ID-PROPERTY "ID")
(defconst ORG-FILE-REGEX "\\`[^.#].*\\.org\\'")
(defconst REVIEW-DUE-PROPERTY "REVIEW_DUE")
(defconst REVIEW-INCREMENT-PROPERTY "REVIEW_INCREMENT")

(defalias 'org-study/start-study 'andy/org-study/start-study)

(defun org-study/review-notes ()
  
  (interactive)

  (let* (
	 (all-files (directory-files-recursively org-directory ORG-FILE-REGEX))

	 ;; Get all headings
         (all-headings
          (cl-mapcan
           (lambda (org-file)
             (with-current-buffer (find-file-noselect org-file)
               (org-map-entries
                (lambda ()

		  (let* ((current-tags (org-get-tags nil t))
			 (id (if (cl-some (lambda (tag)
					    (member tag '("article" "extract" "edit-later")))
					  current-tags)
				 (org-id-get (point) 'create)
			       (org-id-get (point))))))
		  
                  (make-heading
                   :file org-file
                   :id (org-entry-get nil ID-PROPERTY)
		   :text (org-get-heading 'no-todo 'no-tags)
                   :review-due (or (org-entry-get (point) REVIEW-DUE-PROPERTY) (format-time-string "%Y-%m-%d"))
		   :review-increment (or (org-entry-get (point) REVIEW-INCREMENT-PROPERTY) "4")
                   :tags (org-get-tags nil t)))
                t)))
           all-files))
	 
         (headings-filtered-by-tags
          (cl-remove-if-not
           (lambda (heading)
             (cl-intersection (heading-tags heading)
                              '("article" "edit-later" "extract")
                              :test 'equal))
           all-headings))

         (headings-filtered-by-due
          (cl-remove-if-not
           (lambda (heading)
             (let ((review-due (heading-review-due heading))
                   (now (current-time)))
               (time-less-p (org-time-string-to-time review-due) now)))
           headings-filtered-by-tags))

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
		      (lambda (object)
			(let* ((heading-id-STRING (heading-id object))
			       (current-review-due-STRING (heading-review-due object))
			       (current-review-incremental-STRING (heading-review-increment object))
			       (current-review-due-TIMESTAMP (date-to-time current-review-due-STRING))
			       (current-review-incremental-TIMESTAMP
				(days-to-time (string-to-number current-review-incremental-STRING)))
			       (next-review-base-TIMESTAMP
				(if (time-less-p current-review-due-TIMESTAMP (current-time))
				    (current-time)
				  current-review-due-TIMESTAMP))
			       (next-review-due-STRING
				(format-time-string
				 "%Y-%m-%d %H:%M"
				 (time-add
				  next-review-base-TIMESTAMP
				  current-review-incremental-TIMESTAMP)))
			       (next-review-incremental-STRING
				(number-to-string
				 (max 1 (- (string-to-number current-review-incremental-STRING) 1)))))
			  (save-window-excursion
			    (org-id-goto heading-id-STRING)
			    (org-entry-put (point) REVIEW-DUE-PROPERTY next-review-due-STRING)
			    (org-entry-put (point) REVIEW-INCREMENT-PROPERTY next-review-incremental-STRING))
			  (vtable-remove-object (vtable-current-table) object)
			  (message "Interested. Next review due: %s, Next incremental: %s"
				   next-review-due-STRING
				   next-review-incremental-STRING)))
		      "n"
		      (lambda (object)
			(let* ((heading-id-STRING (heading-id object))
			       (current-review-due-STRING (heading-review-due object))
			       (current-review-incremental-STRING (heading-review-increment object))
			       (current-review-due-TIMESTAMP (date-to-time current-review-due-STRING))
			       (current-review-incremental-TIMESTAMP
				(days-to-time (string-to-number current-review-incremental-STRING)))
			       (next-review-base-TIMESTAMP
				(if (time-less-p current-review-due-TIMESTAMP (current-time))
				    (current-time)
				  current-review-due-TIMESTAMP))
			       (next-review-due-STRING
				(format-time-string
				 "%Y-%m-%d %H:%M"
				 (time-add next-review-base-TIMESTAMP current-review-incremental-TIMESTAMP)))
			       (next-review-incremental-STRING
				(number-to-string (max 4 (+ 1 (string-to-number current-review-incremental-STRING))))))
			  (save-window-excursion
			    (org-id-goto heading-id-STRING)
			    (org-entry-put (point) REVIEW-DUE-PROPERTY next-review-due-STRING)
			    (org-entry-put (point) REVIEW-INCREMENT-PROPERTY next-review-incremental-STRING))
			  (vtable-remove-object (vtable-current-table) object)
			  (message "Not interested. Next review due: %s, Next incremental: %s"
				   next-review-due-STRING
				   next-review-incremental-STRING))))
	   :separator-width 3)
	  (setq buffer-read-only t)
	  (switch-to-buffer buffer))))))

(provide 'org-study-api)
