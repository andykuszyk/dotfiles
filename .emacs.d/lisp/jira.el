;;;  jira.el --- a simple wrapper around the JIRA CLI

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

;;; Code

(setq display-buffer-alist '(("jira" display-buffer-same-window)))

(defun jira--parse-issue-reference (text)
  "Tries to parse the JIRA issue reference from the provided text"
  (if (string-match "[A-Z]+\\-[0-9]+" text)
      (match-string 0 text)
    nil))

(defun jira--view-current-issue ()
  "Views the issue represented by the current line"
  (interactive)
  (jira-view-issue (jira--parse-issue-reference (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defun jira--view-in-browser ()
  "Opens the current issue in the browser"
  (interactive)
  (message "not implemented"))

(defun jira--set-keys ()
  "Sets key bindings for JIRA buffers"
  (local-set-key (kbd "q") #'quit-window)
  (local-set-key (kbd "n") #'next-line)
  (local-set-key (kbd "p") #'previous-line)
  (local-set-key (kbd "l") #'jira-list-my-assigned-issues)
  (local-set-key (kbd "a") #'jira-list-my-assigned-issues)
  (local-set-key (kbd "r") #'jira-list-my-reported-issues)
  (local-set-key (kbd "o") #'jira---view-in-browser)
  (local-set-key (kbd "RET") #'jira--view-current-issue))

(defun jira ()
  "Lists issues assigned to you"
  (interactive)
  (jira-list-my-assigned-issues))

(defun jira-list-my-assigned-issues ()
  "Lists issues assigned to you"
  (interactive)
  (jira--list-my-issues "-a"))

(defun jira-list-my-reported-issues ()
  "Lists issues reported by you"
  (interactive)
  (jira--list-my-issues "-r"))

(defun jira--list-my-issues (flag)
  "Lists the issues assigned or reported to you"
  (let ((buffer (get-buffer-create "jira")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (call-process "sh" nil buffer nil "-c" (format "jira issues list %s $(jira me) -s~Done --plain" flag))
      (jira--set-keys)
      (read-only-mode t)
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun jira-view-issue (reference)
  "Prints the description of an issue"
  (interactive "sEnter the issue number: ")
  (if (string= reference "")
      (message "No reference provided")
    (let ((buffer (get-buffer-create "jira")))
      (with-current-buffer buffer
	(read-only-mode -1)
	(erase-buffer)
	(call-process "sh" nil buffer nil "-c" (format "jira issue view %s | cat" reference))
	(setq-local jira--reference reference)
	(jira--set-keys)
	(ansi-color-apply-on-region (point-min) (point-max))
	(read-only-mode t)
	(goto-char (point-min))))))

(provide 'jira)
;;; jira.el ends here
