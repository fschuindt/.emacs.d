(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	(or (bound-and-true-p straight-base-dir)
	    user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package
  emacs
  :config
  (require 'iso-transl)
  (require 'hl-line)
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ring-bell-function 'ignore)
  (setq auto-save-default nil)
  (setq lisp-indent-offset 2)
  (setq linum-format "%d  ")
  (setq enable-recursive-minibuffers t)
  (setq backup-directory-alist `(("." . "~/.emacs_backups")))
  (setq backup-by-copying t)
  (setq delete-old-versions t kept-new-versions 6 kept-old-versions 2 version-control t)
  (global-display-line-numbers-mode 1)
  (set-frame-font "Inconsolata 19" nil t)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (electric-pair-mode t)

  (defun kill-all-buffers ()
    "Reset Emacs to a clean buffer state."
    (interactive)
    (mapc 'kill-buffer (buffer-list)))

  (global-set-key (kbd "C-c k") 'kill-all-buffers))

(straight-use-package 'dockerfile-mode)
(straight-use-package 'heex-ts-mode)

(use-package flatui-theme
  :straight t
  :init
  (defun whitefy ()
    "Use light."
    (interactive)
    (load-theme 'flatui t)
    (set-face-attribute 'mode-line-buffer-id nil :foreground "white")
    (set-face-foreground 'line-number-current-line "#616161")
    (set-face-foreground 'line-number "#a3a3a3")
    (set-face-background 'line-number "#e3e3e3")
    (set-background-color "#ededed")
    (set-foreground-color "#525252")
    (set-face-foreground 'hl-line nil)
    (with-eval-after-load "dired"
      (set-face-foreground 'dired-directory "dark magenta")))
  :config
  (whitefy))

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :init
  (defun blackfy ()
    "Use dark."
    (interactive)
    (load-theme 'sanityinc-tomorrow-bright t)
    (set-face-foreground 'line-number-current-line "#828282")
    (set-face-foreground 'line-number "#444444")
    (set-face-background 'line-number "#161616")
    (set-background-color "#1f1e1e")
    (set-foreground-color "#c7c7c7")
    (set-face-attribute 'region nil :background "#679c86" :foreground "#363636")
    (with-eval-after-load "dired"
      (set-face-foreground 'dired-directory "#4ab597"))))

(use-package dired
  :ensure nil
  :hook (dired-after-readin . sort-directories-first)
  :config
  (defun sort-directories-first ()
    "Sort dired listings with directories first."
    (save-excursion
      (let ((buffer-read-only nil))
	(forward-line 2) ;; skip header lines
	(sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil))))

(use-package wakatime-mode
  :straight t
  :config
  (global-wakatime-mode))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
	      ("<tab>" . copilot-accept-completion)
	      ("TAB" . copilot-accept-completion)))

(straight-use-package 'helm)
(straight-use-package 'helm-ag)

(use-package projectile
  :straight t
  :config
  (setq projectile-enable-caching t)
  (projectile-mode +1)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ido)
  :bind
  (("C-c p" . projectile-command-map)
   ("M-p" . projectile-add-known-project)))

(use-package helm-projectile
  :straight t
  :after (helm projectile)
  :bind
  ("M-n" . helm-projectile-ag))

(use-package ido
  :init
  (setq ido-enable-flex-matching t
	ido-everywhere t
	ido-show-dot-for-dired t
	ido-auto-merge-work-directories-length -1)
  :config
  (ido-mode 1)
  :bind
  ("C-x C-f" . ido-find-file))

(use-package exec-path-from-shell
  :straight t
  :if (or (memq window-system '(mac ns x))
	  (and (display-graphic-p)
	       (getenv "WAYLAND_DISPLAY")))
  :config
  (exec-path-from-shell-initialize))

(use-package elixir-mode
  :straight t
  :hook ((elixir-mode . eglot-ensure)))

(use-package eglot
  :ensure nil
  :config
  (add-to-list
    'eglot-server-programs `((elixir-mode heex-mode) . ("start_lexical.sh")))
  (add-hook
    'elixir-mode-hook 'eglot-ensure)
  (add-hook
    'heex-mode-hook 'eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("M-TAB" . xref-find-definitions)
        ("M-s" . xref-find-references)
        ("C-c a" . eglot-format-buffer)))

(use-package eldoc-box
  :straight t
  :hook (flymake-mode . eldoc-box-hover-at-point-mode)
  :init
  (setq eldoc-box-max-pixel-width 800)
  (setq eldoc-box-max-pixel-height 300)
  (setq eldoc-box-clear-with-C-g t)
  (setq eldoc-box-cleanup-function #'eldoc-box-hover-at-point-cleanup))

(use-package diff-hl
  :straight t
  :hook ((prog-mode . diff-hl-mode)
	 (text-mode . diff-hl-mode)
	 (dired-mode . diff-hl-dired-mode)
	 (dired-mode . dired-hide-details-mode)))

(use-package whitespace
  :straight t
  :hook ((before-save . whitespace-cleanup)
         (prog-mode . whitespace-mode)
         (text-mode . whitespace-mode))
  :custom
  (whitespace-style '(newline-mark tab-mark))
  :config
  (setq whitespace-display-mappings '((newline-mark 10 [172 10]) (indentation 32 [183] [46])))
  (whitespace-mode t))
