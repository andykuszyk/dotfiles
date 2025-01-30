;; I run a single Emacs instance in server mode, and use emacsclient to connect
;; to it if I want an ephemeral editor.
(server-start)

;; Use MELPA as a package repository, and load use-package for managing package
;; configuration.
(org-babel-load-file (concat user-emacs-directory "package-management.org"))

;; Code project management tools.
(org-babel-load-file (concat user-emacs-directory "projects.org"))

;; Configure tools and keybindings for managing Emacs windows.
(org-babel-load-file (concat user-emacs-directory "window-management.org"))

;; Variouus UI customisations, as well as some fundamental usability improvements.
(org-babel-load-file (concat user-emacs-directory "ui.org"))

;; Configure how Emacs stores backup files.
(org-babel-load-file (concat user-emacs-directory "backups.org"))

;; Configure Emacs' environment.
(org-babel-load-file (concat user-emacs-directory "environment.org"))

;; Narrowing and completion tools for the minibuffer, and buffers.
(org-babel-load-file (concat user-emacs-directory "completion.org"))

;; Configure Emacs as a terminal emulator.
(org-babel-load-file (concat user-emacs-directory "terminal.org"))

;; Configure various tools for viewing documentaiton in Emacs.
(org-babel-load-file (concat user-emacs-directory "documentation.org"))

;; Various custom functions and keybindings.
(org-babel-load-file (concat user-emacs-directory "ak.org"))

;; Load some personal packages for interacting with Jira
(org-babel-load-file (concat user-emacs-directory "jira.org"))

;; Specific configuration for Mac OS, and Linux.
(if (string= system-type "darwin")
    (org-babel-load-file (concat user-emacs-directory "darwin.org"))
  (org-babel-load-file (concat user-emacs-directory "linux.org")))

;; General and miscellaneous Emacs configuration.
(org-babel-load-file (concat user-emacs-directory "emacs.org"))

;; Org-mode configuration.
(org-babel-load-file (concat user-emacs-directory "org.org"))

;; Configuration for a variety of different programming languages.
(org-babel-load-file (concat user-emacs-directory "languages.org"))

;; Load configuration for xwidgets, if it was compiled in.
(if (featurep 'xwidget-internal)
    (org-babel-load-file (concat user-emacs-directory "xwidgets.org")))

;; Load my draft package for the gh cli
(when (file-exists-p "~/.emacs.d/lisp/gh-repo-search.el")
    (load-file "~/.emacs.d/lisp/gh-repo-search.el")
    (require 'gh-repo-search))

;; Configure copilot, if it is cloned locally.
(when (file-exists-p "~/repos/copilot.el")
  (org-babel-load-file (concat user-emacs-directory "copilot.org")))

;; Install an xwidget markdown previewer, if it is cloned locally.
(when (and (featurep 'xwidget-internal)
	   (file-exists-p "~/repos/markdown-xwidget"))
  (org-babel-load-file (concat user-emacs-directory "markdown-preview.org")))

;; Load any machine-specific initialisation, if it exists.
(if (file-exists-p "~/.emacs.d/lisp/custom-init.el")
    (load-file "~/.emacs.d/lisp/custom-init.el"))
