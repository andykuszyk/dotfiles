(when (string= system-type "darwin")
  (org-babel-load-file (concat user-emacs-directory "darwin.org")))
(when (string= system-type "gnu/linux")
  (org-babel-load-file (concat user-emacs-directory "linux.org")))
(org-babel-load-file (concat user-emacs-directory "emacs.org"))
