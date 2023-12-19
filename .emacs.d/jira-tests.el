(add-to-list 'load-path "./")
(require 'jira)

(ert-deftest jira--parse-issue-reference-test ()
  (should (string=
	   (jira--parse-issue-reference "Story	NEC-1127	Patching: ECS AMI				In Progress")
	   "NEC-1127"))
  (should (string=
	   (jira--parse-issue-reference "TYPE	KEY		SUMMARY						STATUS")
	   nil))
  (should (string=
	   (jira--parse-issue-reference "Epic	NEC-990		Compute - ECS Hardware Instance Modernisation	In Progress")
	   "NEC-990")))
