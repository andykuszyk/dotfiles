;;;  jira.el --- a simple wrapper around the JIRA CLI -*- lexical-binding: t -*-

;;; Commentary:
;;; This file contains some simple functions that wrap around the JIRA CLI.
;;; You will need the JIRA CLI in order to run them, and it can be installed
;;; from here:
;;;
;;;     https://github.com/ankitpokhrel/jira-cli
;;;
;;; Follow the steps in the README to set-up an API token for the CLI, and
;;; then try running:
;;;
;;;     M-x jira RET.
;;;
;;; This will open a buffer with your assigned issues, with the following
;;; keybindings:
;;;
;;; l    -  list issues assigned to you
;;; r    -  list issues reported to you
;;; RET  -  open a plain-text preview of the issue at point
;;; o    -  open issue in a web browser
;;; w    -  save the current issue reference to the kill ring
;;;
;;; In order for web browser previews to work, you must set the
;;; jira-host variable. For example:
;;;
;;;    (setq jira-host "https://my-org.atlassian.net")

;;; Code:

(require 'ansi-color)

(defvar jira--last-reported-flag)

(defvar jira--current-project
  nil
  "The current project used for filtering results.
If nil the default is used from ~/.config/.jira/.config.yml")

(defvar jira--last-exclude-done t)

(defvar jira-host
  nil
  "The URL of your JIRA instance, e.g. https://my-org.atlassian.net.")

(defvar jira--current-issue-reference
  nil
  "The issue reference of the currently open issue.")

(defvar jira--current-screen
  "list"
  "The name of the screen-type which is currently being displayed.")

(defun jira--parse-issue-reference (text)
  "Try to parse the JIRA issue reference from the provided TEXT."
  (if (string-match "[A-Z]+\\-[0-9]+" text)
      (match-string 0 text)
    nil))

(defun jira--get-current-issue-reference ()
  "Gets the current line's jira issue reference."
  (if (string= jira--current-screen "list")
      (jira--parse-issue-reference
       (buffer-substring-no-properties
	(line-beginning-position)
	(line-end-position)))
    jira--current-issue-reference))

(defun jira--kill-ring-save-reference ()
  "Save the reference of the current issue to the kill ring."
  (interactive)
  (let ((reference (jira--get-current-issue-reference)))
    (message (format "killed reference: %s" reference))
    (kill-new reference)))

(defun jira--view-current-issue ()
  "Views the issue represented by the current line."
  (interactive)
  (jira-view-issue (jira--get-current-issue-reference)))

(defun jira--open-in-browser (reference)
  "Opens the current issue REFERENCE in an external web browser."
  (let ((url (format "%s/browse/%s" jira-host reference)))
    (message (format "opening issue in browser: %s" url))
    (browse-url url)))

(defun jira--view-in-browser ()
  "Opens the current issue in the browser."
  (interactive)
  (jira--open-in-browser (jira--get-current-issue-reference)))

(defun jira ()
  "Lists issues assigned to you."
  (interactive)
  (jira-list-my-assigned-issues))

(defun jira-list-issues-jql (jql)
  "Lists issues found by searching with JQL."
  (interactive "MJQL: ")
  (jira--list-issues nil nil jql))

(defun jira-list-my-assigned-issues ()
  "Lists issues assigned to you."
  (interactive)
  (jira--list-issues "-a" jira--last-exclude-done nil))

(defun jira-list-my-reported-issues ()
  "Lists issues reported by you."
  (interactive)
  (jira--list-issues "-r" jira--last-exclude-done nil))

(defun jira-toggle-exclude-done-issues ()
  "Toggle whether or not to display done issues."
  (interactive)
  (jira--list-issues
   jira--last-reported-flag
   (not  jira--last-exclude-done)
   nil))

(defun jira-set-project (project)
  "Sets the current project to PROJECT."
  (interactive "MProject:")
  (setq jira--current-project project))

(defun jira--set-keys ()
  "Set key bindings for JIRA buffers."
  (local-set-key (kbd "q") #'quit-window)
  (local-set-key (kbd "n") #'next-line)
  (local-set-key (kbd "p") #'previous-line)
  (local-set-key (kbd "P") #'jira-set-project)
  (local-set-key (kbd "l") #'jira-list-my-assigned-issues)
  (local-set-key (kbd "j") #'jira-list-issues-jql)
  (local-set-key (kbd "a") #'jira-list-my-assigned-issues)
  (local-set-key (kbd "r") #'jira-list-my-reported-issues)
  (local-set-key (kbd "x") #'jira-toggle-exclude-done-issues)
  (local-set-key (kbd "o") #'jira--view-in-browser)
  (local-set-key (kbd "w") #'jira--kill-ring-save-reference)
  (local-set-key (kbd "RET") #'jira--view-current-issue))

(defun jira--list-issues (reported-flag exclude-done jql)
  "Lists the issues assigned or reported to you, according to REPORTED-FLAG \\
and EXCLUDE-DONE, or by running the query JQL."
  (setq jira--last-reported-flag reported-flag)
  (setq jira--last-exclude-done exclude-done)
  (setq jira--current-screen "list")
  (let ((buffer (get-buffer-create "jira"))
	(inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (call-process
       "sh"
       nil
       buffer
       nil
       "-c"
       (concat
	"jira issues list"
	(if (not jql) (format " %s $(jira me)" reported-flag) "")
	(if (not jql) (if exclude-done " -s~Done" "") "")
	(if jql (format " --jql '%s'" jql))
	(if jira--current-project
	    (format " --project %s" jira--current-project)
	  "")
	" --plain"
	" --columns 'TYPE,KEY,SUMMARY,STATUS,ASSIGNEE,REPORTER'"))
      (jira--set-keys)
      (read-only-mode t)
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun jira-view-issue (reference)
  "Prints the description of the issue REFERENCE."
  (interactive "sEnter the issue number: ")
  (setq jira--current-screen "view")
  (setq jira--current-issue-reference reference)
  (if (string= reference "")
      (message "No reference provided")
    (let ((buffer (get-buffer-create "jira"))
	  (inhibit-read-only t))
      (with-current-buffer buffer
	(erase-buffer)
	(call-process
	 "sh"
	 nil
	 buffer
	 nil
	 "-c"
	 (format "jira issue view %s | cat" reference))
	(jira--set-keys)
	(ansi-color-apply-on-region (point-min) (point-max))
	(read-only-mode t)
	(goto-char (point-min))
	(display-buffer buffer)))))

(provide 'jira)
;;; jira.el ends here
