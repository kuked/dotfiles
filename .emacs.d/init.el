(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
			  (expand-file-name (concat user-emacs-directory path))))
		(add-to-list 'load-path default-directory)
		(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
			(normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "elisp" "conf" "public_repos")

(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(unless (eq window-system 'ns)
  (menu-bar-mode 0))

(column-number-mode t)
(size-indication-mode t)

(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-x C-m") 'set-mark-command)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(global-auto-revert-mode t)

(load-theme 'nord t)

(require 'open-junk-file)
(setq open-junk-file-format "~/txt/junk/%Y-%m%d-%H%M%S.")
(global-set-key (kbd "C-c C-j") 'open-junk-file)

(require 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

(require 'recentf)
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(setq recentf-max-saved-items 1000)
(setq recentf-exclude '(".recentf"))
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo erea and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))
(run-with-idle-timer 30 t '(lambda ()
                             (with-suppressed-message (recentf-save-list))))
(require 'recentf-ext)

(require 'smartparens-config)
(smartparens-global-mode t)

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (setq company-idle-delay 0)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous))

(ivy-mode t)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-s") 'swiper-isearch)
(counsel-mode t)
(global-set-key (kbd "C-c C-r") 'counsel-recentf)

(require 'yasnippet)

(require 'lsp-mode)
(setq lsp-clients-go-server-args
      '("-format-style=gofmt"))

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc-header t)
(setq lsp-ui-doc-include-signature t)
(setq lsp-ui-sideline-enable nil)
(require 'company-lsp)
(push 'company-lsp company-backends)
(setq company-lsp-cache-candidates t)

(require 'go-mode)
(add-hook 'go-mode-hook #'lsp)
(defun go-before-save-hooks ()
  (when (eq major-mode 'go-mode)
    (lsp-format-buffer)))
(add-hook 'before-save-hook 'go-before-save-hooks)

(require 'golint)

(require 'gotest)
(setq go-test-verbose t)
(define-key go-mode-map (kbd "C-C C-t") 'go-test-current-file)

(require 'ruby-mode)
(add-hook 'ruby-mode-hook #'lsp)

(add-hook 'c++-mode-hook #'lsp)
