;;; gh-repo-search-repo-search.el --- search repos, clone them, and open them.

;;; Commentary:
;; This package provides a simple function for searching for repos in GitHub,
;; cloning them locally, and then browsing them with `dired`.

;;; Code:
(defgroup gh-repo-search
  nil
  "Simple search for GitHub repos."
  :group 'applications
  :prefix "gh-repo-search")

(defcustom gh-repo-search-owner
  nil
  "The owner or organisation to use when invoking the gh CLI."
  :type 'string)

(defcustom gh-repo-search-clone-path
  nil
  "The path to clone repos to."
  :type 'string)

(defun gh-repo-search--kill-name ()
  "Kill the name of the repo on the current line."
  (interactive)
  (let ((line (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (unless (string-match (format "^.*\\(%s/[A-za-z0-9-_]+\\).*" gh-repo-search-owner) line)
      (error "Couldn't find a repo name on the current line"))
    (let ((match (match-string 1 line)))
      match)))

(defun gh-repo-search--open-in-browser ()
  "Open the current repo in a browser."
  (interactive)
  (browse-url (format "https://github.com/%s" (gh-repo-search--kill-name))))

(defun gh-repo-search--clone-repo ()
  "Clone the current repo to `gh-repo-search-clone-path`."
  (interactive)
  (let ((repo (gh-repo-search--kill-name)))
    (when (yes-or-no-p
	   (format  "Are you sure you want to clone the repo %s? " repo))
      (shell-command
       (format
	"cd %s && git clone git@github.com:%s"
	gh-repo-search-clone-path
	repo))
      (message (format "Finished cloning %s" repo)))))

(defun gh-repo-search--dired-repo ()
  "Open the current repo in `dired`."
  (interactive)
  (let* ((repo (gh-repo-search--kill-name))
	 (repo-name (string-trim repo (format "%s/" gh-repo-search-owner)))
	 (repo-path (format "%s/%s" gh-repo-search-clone-path repo-name)))
    (if (file-directory-p repo-path)
	(dired repo-path))))

(defun gh-repo-search--get-clone-status (name)
  "Return whether or not NAME has been cloned into `gh-repo-search-clone-path`."
  (let ((repo-path (format "%s/%s" gh-repo-search-clone-path name)))
    (if (file-directory-p repo-path)
	"Y"
      "N")))

(defun gh-repo-search--get-repo-status (visibility is-archived)
  "Format the repo's status based on VISIBILITY and IS-ARCHIVED."
  (format
   "%s%s"
   visibility
   (if (string= is-archived "t")
       ",archived"
     "")))

(defun gh-repo-search (name)
  "Search for repos in the organisation `gh-repo-search-owner` for NAME."
  (interactive "sRepo name: ")
  (let ((gh-repo-search-buffer (get-buffer-create "*gh*"))
	(json-output
	 (json-parse-string
	  (shell-command-to-string
	   (format
	    "gh search repos --limit 100 --json 'name,description,visibility,isArchived,fullName' --owner %s %s"
	    gh-repo-search-owner
	    name)))))
    (with-current-buffer gh-repo-search-buffer
      (tabulated-list-mode)
      (setq tabulated-list-format
	    (vector
	     '("Cloned?" 10 t . nil)
	     '("Name" 40 t . nil)
	     '("Description" 80 t . nil)
	     '("Status" 30 t . nil)
	     ))
      (setq tabulated-list-entries nil)
      (dolist (repo (append json-output nil))
	(let* ((full-name (gethash "fullName" repo))
	       (cloned (gh-repo-search--get-clone-status (gethash "name" repo)))
	       (description (gethash "description" repo))
	       (status (gh-repo-search--get-repo-status
			(gethash "visibility" repo)
			(gethash "isArchived" repo))))
	  (add-to-list
	   'tabulated-list-entries
	   (list nil (vector cloned full-name description status)))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (local-set-key "w" #'gh-repo-search--kill-name)
      (local-set-key "o" #'gh-repo-search--open-in-browser)
      (local-set-key "C" #'gh-repo-search--clone-repo)
      (local-set-key "d" #'gh-repo-search--dired-repo)
      (local-set-key "s" #'gh-repo-search)
      )
    (display-buffer gh-repo-search-buffer)) )

(setq gh-repo-search-owner "Typeform")
(setq gh-repo-search-clone-path "~/repos/typeform")
(global-set-key (kbd "C-x A h s") #'gh-repo-search)

(provide 'gh-repo-search)
