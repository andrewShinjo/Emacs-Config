;;; file-api.el

(defun andy/file-api/get-files (dir
                                &key
                                file-extensions
                                (exclude-hidden t)
                                (recursive nil))
  (let* ((pattern (if exclude-hidden
                      directory-files-no-dot-files-regexp
                    ".*"))
         (files (if recursive
                    (directory-files-recursively dir pattern)
                  (directory-files dir nil pattern))))
    (if file-extensions
        (cl-remove-if-not
         (lambda (file)
           (member (file-name-extension file) file-extensions))
         files)
      files)))

(provide 'file-api)
