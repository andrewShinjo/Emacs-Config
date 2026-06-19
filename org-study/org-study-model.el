;;; model

(require 'cl-lib)

(cl-defstruct heading
  file
  id
  text
  review-due
  review-increment
  tags)

(provide 'org-study-model)
