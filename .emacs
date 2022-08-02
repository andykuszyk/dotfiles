;; Disable GUI menu bars
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Hide scroll bars
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))

;; Show the time in the modeline
(display-time-mode)
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)

;; Disable splash screen and startup message
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; Inherit environment from default shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs (list "GOROOT" "GOPATH")))

;; Experiment with exwm
(require 'exwm)
(setq exwm-workspace-number 4) ; set 4 as the default number of workspaces
;; Ensure exwm buffers have sensible names
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))
;; Global keybindings
(setq exwm-input-global-keys
      `(
	([?\s-r] . exwm-reset) ; exit char/fullscreen mode
	;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
	([?\s-&] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))
	([?\s-h] . evil-window-left)
	([?\s-l] . evil-window-right)
	([?\s-j] . evil-window-down)
	([?\s-k] . evil-window-up)
	([?\s-H] . evil-window-move-far-left)
	([?\s-L] . evil-window-move-far-right)
	([?\s-J] . evil-window-move-very-bottom)
	([?\s-K] . evil-window-move-very-top)
	([?\s-c] . evil-ex)
	([?\s-s] . evil-window-split)
	([?\s-v] . evil-window-vsplit)
	([?\s-f] . (lambda () (start-process-shell-command "firefox" nil "firefox")))
	))
(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "DP-1" 1 "DP-5" 2 "DP-7"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output DP-0 --off --output DP-1 --mode 1920x1080 --pos 0x840 --rotate normal --output DP-2 --off --output DP-3 --off --output DP-4 --off --output DP-5 --mode 1920x1080 --pos 1920x840 --rotate normal --output DP-6 --off --output DP-7 --mode 1920x1080 --pos 3840x0 --rotate left")))
(exwm-randr-enable)
(exwm-enable)

;; Save backup files in /tmp
(setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))

;; Install MELPA package repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package
(eval-when-compile
  (require 'use-package))

;; Install markdown package
(use-package markdown-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(setq markdown-fontify-code-blocks-natively t)

;; Function for configuring text-width
(defun set-text-width()
  (interactive)
  (auto-fill-mode)
  (set-fill-column 80))

;; Spacemacs theme
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

;; Spaceline status bar
(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))
(spaceline-helm-mode)

;; Use relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Highlight current line
(global-hl-line-mode)

;; Re-map alt-3 to hash on mac keyboard
(define-key key-translation-map (kbd "M-3") (kbd "#"))

;; Evil mode vim emulation
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Fancy battery indicator
(use-package fancy-battery :ensure t)
(add-hook 'after-init-hook #'fancy-battery-mode)
(setq fancy-battery-show-percentage t)

;; Additional vim-style keybindings for window management
(define-key evil-normal-state-map (kbd "C-w <left>") 'windmove-left)
(define-key evil-normal-state-map (kbd "C-w <right>") 'windmove-right)
(define-key evil-normal-state-map (kbd "C-w <up>") 'windmove-u)
(define-key evil-normal-state-map (kbd "C-w <down>") 'windmove-down)

;; vterm terminal emulator
(use-package vterm :ensure t)
(use-package multi-vterm :ensure t)
(defun vterm-hook()
  (display-line-numbers-mode -1)
  (hl-line-mode -1))
(add-hook 'vterm-mode-hook #'vterm-hook)
(add-hook 'multi-vterm-mode-hook #'vterm-hook)
(global-set-key (kbd "C-x v") #'vterm)

;; Treemacs file browser
(use-package treemacs :ensure t)
(use-package treemacs-evil :ensure t)
(use-package treemacs-projectile :ensure t)
(global-set-key [f8] #'treemacs)

;; Projectile
(use-package projectile
    :ensure t
    :init
    (projectile-mode +1)
    :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; Emacs application framework
(defun eaf-enabled ()
  nil)
(when (and (eaf-enabled) (file-directory-p "~/.emacs.d/site-lisp/emacs-application-framework/"))
    (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
    (require 'eaf)
    (require 'eaf-browser)
    (require 'eaf-mermaid)
    (require 'eaf-terminal))

;; Helm completion
(use-package helm :ensure t)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;; Magit
(use-package magit :ensure t)
(setq magit-diff-refine-hunk (quote all)) ; Use word diffs when showing diffs

;; LSP
(use-package lsp-mode :ensure t)

;; Auto-completion
(use-package company :ensure t)
(add-hook 'after-init-hook #'global-company-mode) ; Enable company mode in all buffers

;; Go support
(use-package go-mode :ensure t)
(add-hook 'go-mode-hook #'lsp)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)

;; Terraform support
(use-package terraform-mode :ensure t)
(add-hook 'terraform-mode-hook #'lsp)
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;; YAML support
(use-package yaml-mode :ensure t)

;; Function for synchronising notes files with git
(defun sync-notes()
  (when (string-match-p (regexp-quote "notes") buffer-file-name)
    (magit-stage-file buffer-file-name)
    (magit-commit-create (list "-m" "auto-commit from emacs"))
    (magit-fetch-all ())
    (magit-rebase-branch "origin/master" ())
    (magit-push-current-to-upstream ())))

;; Auto commit/push files after saving for notes repos.
(add-hook 'after-save-hook #'sync-notes)

;; Org mode customisation
(setq org-startup-folded t) ; open org files folded, rather than expanded
(setq org-edit-src-content-indentation 0) ; do not indent code in source blocks

;; FZF
(use-package fzf :ensure t)

;; Convenience make function
;; Alternatively, `M-!, make` can be used.
(defun make ()
  (interactive)
  (shell-command "make &> /dev/null" nil))

;; Perspective
(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

;; Dockerfile mode
(use-package dockerfile-mode :ensure t)
(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))

;; Numbered window switcher
(use-package ace-window :ensure t)

;; Better JSON support
(use-package json-mode :ensure t)
