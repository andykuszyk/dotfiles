(defun jira-list-my-issues ()
  "Lists the issues assigned to you"
  (interactive)
  (let ((buffer (get-buffer-create "jira list")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (kill-region (point-min) (point-max)))
    (call-process "sh" nil buffer nil "-c" "jira issues list -a $(jira me) -s~Done --plain")
    (with-current-buffer buffer
      (read-only-mode nil))
    (display-buffer buffer)))

(defun jira-view-issue (reference)
  "Prints the description of an issue"
  (interactive "sEnter the issue number: ")
  (shell-command (format "jira issue view %s | cat" reference)))

;;; jira.el ends here
