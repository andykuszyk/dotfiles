; Disable GUI menu bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; Always show tab bar
(tab-bar-mode 1)

; Install MELPA package repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; Install use-package
(eval-when-compile
  (require 'use-package))

; Install markdown package
(use-package markdown-mode :ensure t)

; Spacemacs theme
(use-package spacemacs-theme :ensure t)
(load-theme 'spacemacs-dark t)

; Use relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

; Highlight current line
(global-hl-line-mode)

; Re-map alt-3 to hash on mac keyboard
(define-key key-translation-map (kbd "M-3") (kbd "#"))

; Evil mode vim emulation
(use-package evil :ensure t)
(require 'evil)
(evil-mode 1)

; vterm terminal emulator
(use-package vterm :ensure t)
(use-package multi-vterm :ensure t)

; Powerline
(require 'powerline)
(powerline-center-evil-theme)

; Neotree file browser
(use-package neotree :ensure t)

; Projectile
(use-package projectile
    :ensure t
    :init
    (projectile-mode +1)
    :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

; Emacs application framework
(if (file-directory-p "~/.emacs.d/site-lisp/emacs-application-framework/")
    (progn
	(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
	(require 'eaf)))

; Helm completion
(use-package helm :ensure t)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)
