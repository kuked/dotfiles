(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(eval-when-compile (require 'use-package))

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


(menu-bar-mode 0)
(tool-bar-mode 0)
(column-number-mode t)
(size-indication-mode t)
(when (eq system-type "darwin")
  (setq mac-option-modifier 'meta))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq mac-option-modifier 'meta)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(use-package nord-theme
  :config
  (load-theme 'nord t))

(use-package smartparens-config
  :init
  (smartparens-global-mode))


(use-package recentf
  :init
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude '(".recentf"))
  :config
  (use-package recentf-ext))

(defmacro with-suppressed-message (&rest body)
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(run-with-idle-timer 30 t '(lambda ()
                             (with-suppressed-message (recentf-save-list))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))



;; dumb-jump
;; M-.(go), M-,(back)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)


(add-hook 'c-mode-hook '(lambda ()
                          (setq c-basic-offset 4)))

(use-package ruby-mode
  :commands ruby-mode
  :mode (("\\.rb$"    . ruby-mode)
         ("Gemfile$"  . ruby-mode)
         ("Rakefile$" . ruby-mode)))


(use-package company
  :init
  (global-company-mode)
  (setq company-idle-delay 0)
  :bind
  (:map company-active-map
	("C-n" . company-select-next)
	("C-p" . company-select-previous))
  (:map company-search-map
	("C-n" . company-select-next)
	("C-p" . company-select-previous)))


(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  :bind
  ("C-c C-r" . counsel-recentf)
  ("C-s"     . swiper)
  ("C-x C-f" . counsel-find-file)
  ("M-x"     . counsel-M-x))


(use-package popwin
  :init (popwin-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; #rust

(use-package rust-mode
  :ensure t
  :custom rust-format-on-save t)

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; #elm

(use-package elm-mode
  :ensure t
  :hook (elm-mode . elm-format-on-save-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :init (yas-global-mode)
  :hook ((ruby-mode . lsp)
         (rust-mode . lsp))
  :bind ("C-c h" . lsp-describe-thing-at-point)
  :commands lsp
  :custom  ((lsp-enable-indentation nil)
            (lsp-rust-server 'rust-analyzer)))

(use-package lsp-ui
  :ensure t)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)


(bind-key "C-m"     'newline-and-indent)
(bind-key "C-x C-m" 'set-mark-command)
(bind-key [f12] 'toggle-frame-fullscreen)

;; https://gist.github.com/lateau/5260613
(defun dont-kill-emacs()
  "Disable \\[kill-emacs] binding execute."
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))
(global-set-key (kbd "C-x C-c") 'dont-kill-emacs)

(defun toggle-calendar ()
  "Toggle calendar display/hide."
  (interactive)
  (if (get 'toggle-calendar 'state)
      (progn (calendar-exit) (put 'toggle-calendar 'state nil))
      (progn (calendar) (put 'toggle-calendar 'state t))))
(global-set-key (kbd "<f7>") 'toggle-calendar)

(bind-key [f5] '(lambda ()
                  (interactive)
                  (insert (format-time-string "%Y-%m-%d"))))

(defun sunny ()
  (interactive)
  (insert "☀️"))


;; change custom file location
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
