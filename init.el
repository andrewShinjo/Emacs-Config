;;; init.el

;; Package Manager

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;;; Imports

(let ((config-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "org-study" config-dir))
  (load "org-study-api"))

;;; My functions

(defun andy/open-init-file ()
  (interactive)
  (find-file user-init-file))

;;; Emacs

(setq make-backup-files nil)
(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)
(tool-bar-mode -1)

(set-frame-parameter nil 'fullscreen 'maximized)

;;; Org mode

(setq org-directory (expand-file-name "~/Documents/Org"))

;;; Org Roam

(setq org-roam-directory org-directory)
(org-roam-db-autosync-mode)
