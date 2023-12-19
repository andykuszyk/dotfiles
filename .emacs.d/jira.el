;;;  jira.el --- a simple wrapper around the JIRA CLI

;;; Commentary:

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

(defun jira--set-keys ()
  "Sets key bindings for JIRA buffers"
  (local-set-key (kbd "q") #'quit-window)
  (local-set-key (kbd "n") #'next-line)
  (local-set-key (kbd "p") #'previous-line)
  (local-set-key (kbd "l") #'jira-list-my-issues)
  (local-set-key (kbd "RET") #'jira--view-current-issue))

(defun jira-list-my-issues ()
  "Lists the issues assigned to you"
  (interactive)
  (let ((buffer (get-buffer-create "jira")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (call-process "sh" nil buffer nil "-c" "jira issues list -a $(jira me) -s~Done --plain")
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
	(jira--set-keys)
	(ansi-color-apply-on-region (point-min) (point-max))
	(read-only-mode t)
	(goto-char (point-min))))))

(provide 'jira)
;;; jira.el ends here
