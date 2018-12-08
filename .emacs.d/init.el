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

;; バックアップとオートセーブファイルを~/.emacs.d/backupsに集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; C-SPCはスポットライトにわりあてているので
(global-set-key (kbd "C-x C-m") 'set-mark-command)

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
(smartparens-global-mode)

;; popwin
(require 'popwin)
(popwin-mode 1)

;; lua-mode
(require 'lua-mode)

;; c-mode
(add-hook 'c-mode-hook
          '(lambda()
             (setq-default sp-escape-quotes-after-insert nil)))

;; whitespace
(require 'whitespace)
(setq whitespace-style '(face tabs trailing tab-mark))
(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
(set-face-attribute 'whitespace-trailing nil
                    :foreground "RoyalBlue4"
                    :background "RoyalBlue4"
                    :underline nil)
(set-face-attribute 'whitespace-tab nil
                    :foreground "yellow4"
                    :background "yellow4"
                    :underline nil)
(global-whitespace-mode t)


;; python
(require 'py-autopep8)
(setq py-autopep8-options '("--max-line-length=150"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; slim
(require 'slim-mode)

;; yaml
(require 'yaml-mode)

;; open-junk-file
(require 'open-junk-file)
(global-set-key (kbd "C-x C-j") 'open-junk-file)

;; lispxmp
(require 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

;; paredit
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
