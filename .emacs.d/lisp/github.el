;;; github.el --- a simple wrapper around the GitHub CLI -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

(require 'ob-core)

(defun github--clean-src-body (body)
  (string-replace "%" "%%" (string-replace "'" "'\"'\"'" body)))

(defun github-src-block-edit ()
  (interactive)
  (let* ((src-block-info (org-babel-get-src-block-info))
	 (src-block-body (nth 1 src-block-info))
	 (github-issue (cdr (assoc :github-issue (nth 2 src-block-info))))
	 (github-owner (cdr (assoc :github-owner (nth 2 src-block-info))))
	 (github-repo (cdr (assoc :github-repo (nth 2 src-block-info))))
	 (github-title (cdr (assoc :github-title (nth 2 src-block-info)))))
    (if (not github-issue)
	(error ":github-issue is a required header argument"))
    (if (not github-owner)
	(error ":github-owner is a required header argument"))
    (if (not github-repo)
	(error ":github-repo is a required header argument"))
    (let ((github-cmd (format
		       "gh issue edit %d -R %s/%s %s -b '%s'"
		       github-issue
		       github-owner
		       github-repo
		       (if github-title
			   (format "-t %s" github-title)
			 "")
		       (github--clean-src-body src-block-body))))
      (message github-cmd)
      (shell-command github-cmd))
    ))

(defun github--open-in-browser (issue owner repo)
  (browse-url (format "https://github.com/%s/%s/issues/%d"
		      owner
		      repo
		      issue)))

(defun github-src-block-open ()
  (interactive)
  (let* ((src-block-info (org-babel-get-src-block-info))
	 (github-owner (cdr (assoc :github-owner (nth 2 src-block-info))))
	 (github-repo (cdr (assoc :github-repo (nth 2 src-block-info))))
	 (github-issue (cdr (assoc :github-issue (nth 2 src-block-info)))))
    (if (not github-issue)
	(error ":github-issue is a required header argument"))
    (if (not github-owner)
	(error ":github-owner is a required header argument"))
    (if (not github-repo)
	(error ":github-repo is a required header argument"))
    (github--open-in-browser github-issue github-owner github-repo)))

(defvar-keymap github-keymap
  "o" #'github-src-block-open  
  "e" #'github-src-block-edit)

(global-set-key (kbd "C-x G") github-keymap)

(provide 'github)
;;; github.el ends here
