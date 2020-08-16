;; init.el

(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


;; https://ayatakesi.github.io/emacs/24.5/Saving-Customizations.html
(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)


(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


(unless (eq window-system 'ns) (menu-bar-mode 0))
(column-number-mode t)
(size-indication-mode t)


(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)


;; nord-emacs
;; https://github.com/arcticicestudio/nord-emacs
(load-theme 'nord t)


;; company
;; http://company-mode.github.io
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (setq company-idle-delay 0)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous))


;; recentf
;; https://qiita.com/blue0513/items/c0dc35a880170997c3f5
(require 'recentf)
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(setq recentf-max-saved-items 1000)
(setq recentf-exclude '(".recentf"))
(defmacro with-suppressed-message (&rest body)
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))
(run-with-idle-timer 30 t '(lambda ()
                             (with-suppressed-message (recentf-save-list))))

;; recentf-ext
;; https://github.com/rubikitch/recentf-ext
(require 'recentf-ext)


;; smartparens
;; https://github.com/Fuco1/smartparens
(require 'smartparens-config)
(smartparens-global-mode)


;; popwin
;; https://github.com/emacsorphanage/popwin
(require 'popwin)
(popwin-mode 1)


;; swiper
;; https://github.com/abo-abo/swiper
(ivy-mode t)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)


;; flycheck
(setq flycheck-highlighting-mode 'nil)
(setq-default flycheck-indication-mode 'left-margin)
(add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)
(defun my/set-flycheck-margins ()
  (setq left-fringe-width 8 right-fringe-width 8
        left-margin-width 1 right-margin-width 0)
  (flycheck-refresh-fringes-and-margins))
(add-hook 'flycheck-mode-hook #'my/set-flycheck-margins)
(setq flycheck-display-errors-delay 0.3)


;; python
(add-hook 'python-mode-hook #'lsp)
(add-hook 'python-mode-hook #'flycheck-mode)


;; py-yapf
;; https://github.com/paetzke/py-yapf.el
(require 'py-yapf)
(add-hook 'python-mode-hook 'py-yapf-enable-on-save)


;; rust
(require 'rust-mode)
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'flycheck-mode)
(setq rust-format-on-save t)
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)


;; ruby-electric
(require 'ruby-electric)
(add-hook 'ruby-mode-hook #'ruby-electric-mode)

;; slime
(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-fancy slime-company))

;; original
;; https://rion778.hatenablog.com/entry/20100926/1285488467
(defun my-slime ()
  (interactive)
  (if (< (count-windows) 2)
      (split-window-horizontally))
  (slime)
  (other-window 1))


;; dumb-jump
;; https://github.com/jacktasia/dumb-jump
;; M-.(go), M-,(back)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)


;; global-set-keys
(global-set-key (kbd "C-c C-r") 'counsel-recentf)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-x C-m") 'set-mark-command)
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)
(define-key global-map (kbd "<f5>")
  '(lambda ()
     (interactive)
     (insert (format-time-string "%H:%M %Y/%m/%d"))))

(defun toggle-calendar ()
  (interactive)
  (if (get 'toggle-calendar 'state)
      (progn (calendar-exit)
             (put 'toggle-calendar 'state nil))
      (progn (calendar)
             (put 'toggle-calendar 'state t))))
(global-set-key (kbd "<f7>") 'toggle-calendar)
