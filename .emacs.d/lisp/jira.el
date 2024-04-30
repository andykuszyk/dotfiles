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
(require 'ob-core)

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

(defun jira--clean-src-body (body)
  (string-replace "%" "%%" (string-replace "'" "'\"'\"'" body)))

(defun jira-src-block-edit ()
  "Edit a Jira issue using the contents of an \"org-mode\" source block.

This function takes the contents of a Markdown source block, and edits a
matching Jira issue based on the :jira-key header argument.  The title and
custom fields can also be edited.  The header arguments supported by this
function are as follows:

  :jira-key      This indicates that the source block is intended to be a Jira
                 issue.  It should be set to \"new\" when this function is
                 executed.

  :jira-title    The title/summary of the Jira issue will be taken from this
                 header argument.

  :jira-project  The project to edit the Jira issue in.  This argument is
                 optional, and if omitted will be taken from
                 ~/.config/.jira/.config.yml.

  :jira-custom   Custom fields that should be set when creating the issue in the
                 format field_name=field_value.

For example, when invoked on the source block below, the title will be updated
to \"foo\", and the body will be updated to \"bar\":

  #+begin_src markdown :jira-key EMX-123 :jira-title foo
  bar
  #+end_src"
  (interactive)
  (let* ((src-block-info (org-babel-get-src-block-info))
	 (src-block-body (nth 1 src-block-info))
	 (jira-key (cdr (assoc :jira-key (nth 2 src-block-info))))
	 (jira-title (cdr (assoc :jira-title (nth 2 src-block-info))))
	 (jira-project (cdr (assoc :jira-project (nth 2 src-block-info))))
	 (jira-custom (cdr (assoc :jira-custom (nth 2 src-block-info)))))
    (if (not jira-key)
	(message ":jira-key header argument is required")
      (let ((jira-cmd (format
		       "jira issue edit %s %s %s %s --no-input -b '%s'"
		       jira-key
		       (if jira-title (format "-s '%s' " jira-title) "")
		       (if jira-project (format "-p '%s' " jira-project) "")
		       (if jira-custom (format "--custom '%s' " jira-custom) "")
		       (jira--clean-src-body src-block-body))))
	(message jira-cmd)
	(shell-command jira-cmd)))))

(defun jira-src-block-new ()
  "Create a new Jira issue from an \"org-mode\" source block.

This function takes the contents of a Markdown source block, and creates a new
issue according to the source block's header arguments.  The header arguments
that are supported are as follows:

  :jira-key      This indicates that the source block is intended to be a Jira
                 issue.  It should be set to \"new\" when this function is
                 executed.

  :jira-title    The title/summary of the Jira issue will be taken from this
                 header argument.

  :jira-type     The type of the Jira issue to be created.  Defaults to
                 \"Story\"', but could also be set to \"Epic\" or \"Task\"'.

  :jira-project  The project to create the Jira issue in.  This argument is
                 optional, and if omitted will be taken from
                 ~/.config/.jira/.config.yml.

  :jira-custom   Custom fields that should be set when creating the issue in the
                 format field_name=field_value.

  :jira-parent   The parent issue key (e.g. epic) of the issue to be created.

For example, this when this function is invoked for the source block below, a
new issue will be created with the title \"foo\", the body \"bar\", the type
\"Story\":

  #+begin_src markdown :jira-key new :jira-title foo :jira-type Story
  bar
  #+end_src

The issue is created using the Jira CLI, whose output is printed in the
minibuffer. If creating the issue was successful, the new issue key will be
reported there. Don't forget to update the :jira-key header argument before
making future edits."
  (interactive)
  (let* ((src-block-info (org-babel-get-src-block-info))
	 (src-block-lang (nth 0 src-block-info))
	 (src-block-body (nth 1 src-block-info))
	 (jira-key (cdr (assoc :jira-key (nth 2 src-block-info))))
	 (jira-title (cdr (assoc :jira-title (nth 2 src-block-info))))
	 (jira-type (cdr (assoc :jira-type (nth 2 src-block-info))))
	 (jira-project (cdr (assoc :jira-project (nth 2 src-block-info))))
	 (jira-parent (cdr (assoc :jira-parent (nth 2 src-block-info))))
	 (jira-custom (cdr (assoc :jira-custom (nth 2 src-block-info)))))
        (if (not (string-equal "markdown" src-block-lang))
	(message "the language of the source block must be 'markdown'")
      (if (not jira-key)
	  (message ":jira-key header argument is required")
	(if (not (string-equal "new" jira-key))
	    (message ":jira-key must be 'new' when creating a new issue")
	  (if (not jira-title)
	      (message ":jira-title is required for creating issues")
	    (let ((jira-cmd
		   (format
		    "jira issue create --no-input %s %s %s -s '%s' -t '%s' -b '%s'"
		    (if jira-parent (format "-P %s " jira-parent) "")
		    (if jira-project (format "-p '%s' " jira-project) "")
		    (if jira-custom (format "--custom '%s' " jira-custom) "")
		    jira-title
		    (if jira-type jira-type "Story")
		    (jira--clean-src-body src-block-body))))
	      (message jira-cmd)
	      (shell-command jira-cmd))))))))

(defun jira-src-block-open ()
  "Open the Jira issue associated with the current source block in a browser.

This function opens the Jira issue associated with the current \"org-mode\"
source block based on its :jira-key header argument.

For example, if this function was invoked on the source block below, issue
EMX-123 would be opened in the system`s default web browser:

  #+begin_src markdown :jira-key EMX-123
  Emacs rocks!
  #+end_src

The URL of the Jira issue is constructed using the \"jira-host\" variable."
  (interactive)
  (let* ((src-block-info (org-babel-get-src-block-info))
	 (jira-issue (cdr (assoc :jira-key (nth 2 src-block-info)))))
    (if (not jira-issue)
	(message "jira issue could not be found!")
      (jira--open-in-browser jira-issue))))

(defun jira-src-block-kill-key ()
  "Kill the issue key of the current source block to the kill ring.

When invoked on a source block with the :jira-key header argument, this function
will add the issue key to the kill ring.

For example, if called on the source block below, \"EMX-123\" would be added to
the kill ring:

  #+begin_src markdown :jira-key EMX-123
  Plain text for the win!
  #+end_src"
  (interactive)
  (let* ((src-block-info (org-babel-get-src-block-info))
	 (jira-issue (cdr (assoc :jira-key (nth 2 src-block-info)))))
    (if (not jira-issue)
	(message "jira issue could not be found!")
      (message (format "killed reference: %s" jira-issue))
      (kill-new jira-issue))))

(defun jira-list-epics ()
  (interactive)
  (let ((buffer (get-buffer-create "*jira epics*"))
	(inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (call-process
       "sh"
       nil
       buffer
       nil
       "-c"
       "jira epic list --plain --table")
      (jira--set-keys)
      (read-only-mode t)
      (goto-char (point-min)))
    (display-buffer buffer)))

(defvar-keymap jira-keymap
  "j" #'jira
  "e" #'jira-src-block-edit
  "E" #'jira-list-epics
  "o" #'jira-src-block-open
  "n" #'jira-src-block-new
  "w" #'jira-src-block-kill-key
  "p" #'jira-set-project)

(global-set-key (kbd "C-x j") jira-keymap)

(provide 'jira)
;;; jira.el ends here
