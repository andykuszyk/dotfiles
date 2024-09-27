(defcustom gh-owner
  nil
  "The owner or organisation to use when invoking gh.
The value of this variable is case-sensitive, and must match the case used in
  GitHub."
  :type 'string)

(defcustom gh-clone-path
  nil
  "The path to clone repos to."
  :type 'string)

(setq gh-owner "Typeform")
(setq gh-clone-path "~/repos/typeform")

(defun gh-search-repos (name)
  "Search github repos for `gh-owner` with the given NAME."
  (interactive "sRepo name: ")
  (let ((gh-buffer (get-buffer-create "*gh*")))
    (shell-command
     (format  "gh search repos --owner %s %s" gh-owner name)
     gh-buffer)
    (with-current-buffer gh-buffer
      (special-mode)
      (read-only-mode)
      (local-set-key "w" #'gh-kill-name)
      (local-set-key "o" #'gh-open-in-browser)
      (local-set-key "C" #'gh-clone-repo)
      (local-set-key "d" #'gh-dired-repo)
      )
    (display-buffer gh-buffer)))

(defun gh-kill-name ()
  (interactive)
  (let ((line (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (unless (string-match (format "^.*\\(%s/[A-za-z0-9-_]+\\).*" gh-owner) line)
      (error "Couldn't find a repo name on the current line"))
    (let ((match (match-string 1 line)))
      (message (format "Killed repo name: %s" match))
      match)))

(defun gh-open-in-browser ()
  (interactive)
  (browse-url (format "https://github.com/%s" (gh-kill-name))))

(defun gh-clone-repo ()
  (interactive)
  (let ((repo (gh-kill-name)))
    (when (yes-or-no-p
	   (format  "Are you sure you want to clone the repo %s? " repo))
      (shell-command
       (format
	"cd %s && git clone git@github.com:%s"
	gh-clone-path
	repo)))))

(defun gh-dired-repo ()
  (interactive)
  (let* ((repo (gh-kill-name))
	 (repo-name (string-trim repo (format "%s/" gh-owner)))
	 (repo-path (format "%s/%s" gh-clone-path repo-name)))
    (message repo-path)
    (if (file-directory-p repo-path)
	(dired repo-path))))

(global-set-key (kbd "C-x A h s") #'gh-search-repos)
(global-set-key (kbd "C-x A h j") #'gh-search-repos-json)

(defun gh-search-repos-json (name)
  (interactive "sRepo name: ")
  (let ((gh-buffer (get-buffer-create "*gh*"))
	(json-output
	 (json-parse-string
	  (shell-command-to-string
	   (format
	    "gh search repos --json 'name,description,visibility,isArchived,fullName' --owner %s %s"
	    gh-owner
	    name)))))
    (with-current-buffer gh-buffer
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
	       (cloned (gh--get-clone-status (gethash "name" repo)))
	       (description (gethash "description" repo))
	       (status (gh--get-repo-status
			(gethash "visibility" repo)
			(gethash "isArchived" repo))))
	  (add-to-list
	   'tabulated-list-entries
	   (list nil (vector cloned full-name description status)))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (local-set-key "w" #'gh-kill-name)
      (local-set-key "o" #'gh-open-in-browser)
      (local-set-key "C" #'gh-clone-repo)
      (local-set-key "d" #'gh-dired-repo)
      )
    (display-buffer gh-buffer)) )

(defun gh--get-clone-status (name)
  (let ((repo-path (format "%s/%s" gh-clone-path name)))
    (if (file-directory-p repo-path)
	"Y"
      "N")))

(defun gh--get-repo-status (visibility is-archived)
  (format
   "%s%s"
   visibility
   (if (string= is-archived "t")
       ",archived"
     "")))

(provide 'gh)
