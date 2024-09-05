(server-start)

(org-babel-load-file (concat user-emacs-directory "package-management.org"))
(org-babel-load-file (concat user-emacs-directory "window-management.org"))
(org-babel-load-file (concat user-emacs-directory "ui.org"))
(org-babel-load-file (concat user-emacs-directory "backups.org"))
(org-babel-load-file (concat user-emacs-directory "environment.org"))
(org-babel-load-file (concat user-emacs-directory "projects.org"))
(org-babel-load-file (concat user-emacs-directory "completion.org"))
(if (string= system-type "darwin")
    (org-babel-load-file (concat user-emacs-directory "darwin.org"))
  (org-babel-load-file (concat user-emacs-directory "linux.org")))
(org-babel-load-file (concat user-emacs-directory "emacs.org"))
(org-babel-load-file (concat user-emacs-directory "org.org"))
(org-babel-load-file (concat user-emacs-directory "languages.org"))
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'jira)
(require 'github)
(if (file-exists-p "~/.emacs.d/lisp/custom-init.el")
    (load-file "~/.emacs.d/lisp/custom-init.el"))
