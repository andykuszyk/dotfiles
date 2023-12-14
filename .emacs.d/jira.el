(defun jira-list-my-issues ()
  "Lists the issues assigned to you"
  (interactive)
  (shell-command "jira issues list -a $(jira me) -s~Done --plain"))

(defun jira-view-issue (reference)
  "Prints the description of an issue"
  (interactive "sEnter the issue number: ")
  (shell-command (format "jira issue view %s | cat" reference)))

;;; jira.el ends here
