; Disable GUI menu bars
(menu-bar-mode -1)
(tool-bar-mode -1)

; Install MELP package repository
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

; Install markdown package
(use-package markdown-mode :ensure t)

; Dark theme
(load-theme 'deeper-blue t)

; Use relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

; Highlight current line
(global-hl-line-mode)

; Re-map alt-3 to hash on mac keyboard
(define-key key-translation-map (kbd "M-3") (kbd "#"))
