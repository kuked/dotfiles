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

;; package-installするとinit.elが更新されるのがしんどい
;; https://www.reddit.com/r/emacs/comments/4x655n/packageselectedpackages_always_appear_after/
(setq custom-file "~/.emacs.d/package-selected-packages.el")
(load custom-file)

(require 's)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 200)
(setq recentf-auto-cleanup 'never)

;; ivy
(ivy-mode 1)
(setq ivy-height 15)
(counsel-mode 1)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c C-r") 'counsel-recentf)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

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
(setq lua-indent-level 2)

;; http://puntoblogspot.blogspot.com/2018/03/fixing-indentation-of-lua-busted-in.html
(defun lua-busted-indentation-fix ()
  (save-excursion
    (lua-forward-line-skip-blanks 'back)
    (let* ((current-indentation (current-indentation))
           (line (thing-at-point 'line t))
           (busted-p (s-matches?
                      (rx (+ bol (* space)
                             (or "context" "describe" "it" "setup" "teardown")
                             "("))
                      line)))
          (when busted-p
            (+ current-indentation lua-indent-level)))))

(defun rgc-lua-calculate-indentation-override (old-function &rest arguments)
  (or (lua-busted-indentation-fix)
      (apply old-function arguments)))

(advice-add #'lua-calculate-indentation-override
            :around #'rgc-lua-calculate-indentation-override)


;; c-mode
(add-hook 'c-mode-hook
          '(lambda()
             (setq-default sp-escape-quotes-after-insert nil)))

;; whitespace
(require 'whitespace)
(setq whitespace-style '(face tabs trailing))
;; (setq whitespace-display-mappings
;;       '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
(set-face-attribute 'whitespace-trailing nil
                    :foreground "RoyalBlue4"
                    :background "RoyalBlue4"
                    :underline nil)
(global-whitespace-mode t)

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

;; go
(add-to-list 'exec-path (expand-file-name "/Users/kuked/dev/bin/"))
(require 'go-mode)
(add-hook 'go-mode-hook
          (lambda()
            (setq c-basic-offset 4)
            (setq tab-width 4)
            (add-hook 'before-save-hook 'gofmt-before-save)
            (local-set-key (kbd "M-.") 'godef-jump)
            (set (make-local-variable 'company-backends) '(company-go))))

(require 'company-go)

;; php
(require 'php-mode)

;; java
(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
