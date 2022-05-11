(menu-bar-mode -1)
(tool-bar-mode -1)

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(use-package markdown-mode :ensure t)
(load-theme 'deeper-blue t)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(global-hl-line-mode)
