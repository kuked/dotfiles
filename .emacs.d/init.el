(require 'package)
(add-to-list
 'package-archives
 '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; turn off welcome screen
(setq inhibit-startup-message t)

;; disable the menu bar
(menu-bar-mode -1)

;; show column-number in mode line
(column-number-mode t)

;; turn on parentheses highlighting
(show-paren-mode)

;; no tabs
(setq-default indent-tabs-mode nil)

;; helm
(require 'helm-config)
(helm-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)

;; ruby
(require 'ruby-mode)

;; ruby-electric
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

;; haml
(require 'haml-mode)

;; slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime)
(setq slime-contribs '(slime-fancy))

;; nord-theme
(load-theme 'nord t)

;; smartparens
(require 'smartparens-config)
(add-hook 'lisp-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)

;; popwin
(require 'popwin)
(popwin-mode 1)

;; lua-mode
(require 'lua-mode)
(add-hook 'lua-mode-hook #'smartparens-mode)
