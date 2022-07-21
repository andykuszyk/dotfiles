;; Disable GUI menu bars
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Hide scroll bars
(defun scroll-bar-hook (frame)
  (when (display-graphic-p)
    (scroll-bar-mode -1)))
(add-hook 'after-make-frame-functions #'scroll-bar-hook)

;; Always show tab bar
(tab-bar-mode 1)

;; Show the time in the modeline
(display-time-mode)
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)

;; Disable splash screen and startup message
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; Add Go and local bin paths to the exec path
(let ((go-bin-path "~/go/bin")
      (local-bin-path "~/.local/bin")
      (go-root-bin "/usr/local/go/bin"))
      (setq exec-path (append exec-path (list go-bin-path local-bin-path go-root-bin)))
      (setenv "PATH" (concat (getenv "PATH") ":" go-bin-path ":" local-bin-path ":" go-root-bin)))
(setenv "GOPATH" "~/go")
(setenv "GOROOT" "/usr/local/go")

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

;; Neotree file browser
(use-package neotree :ensure t)
(defun neotree-hook()
  (display-line-numbers-mode -1)
  (define-key evil-normal-state-local-map (kbd "TAB") #'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "RET") #'neotree-enter))
(add-hook 'neotree-mode-hook 'neotree-hook)
(setq-default neo-show-hidden-files t)
(global-set-key [f8] #'neotree-toggle)

;; Projectile
(use-package projectile
    :ensure t
    :init
    (projectile-mode +1)
    :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; Emacs application framework
(if (file-directory-p "~/.emacs.d/site-lisp/emacs-application-framework/")
    (progn
	(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
	(require 'eaf)
	(require 'eaf-browser)))

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
(setq org-startup-folded t)

;; FZF
(use-package fzf :ensure t)

;; Convenience make function
(defun make ()
  (interactive)
  (shell-command "make"))
