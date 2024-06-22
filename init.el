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

  (defun kill-all-buffers ()
    "Reset Emacs to a clean buffer state."
    (interactive)
    (mapc 'kill-buffer (buffer-list)))

  (global-set-key (kbd "C-c k") 'kill-all-buffers))

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

(use-package treesit
  :init
  (setq treesit-language-source-alist
    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
       (cmake "https://github.com/uyha/tree-sitter-cmake")
       (css "https://github.com/tree-sitter/tree-sitter-css")
       (elisp "https://github.com/Wilfred/tree-sitter-elisp")
       (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
       (go "https://github.com/tree-sitter/tree-sitter-go")
       (heex "https://github.com/phoenixframework/tree-sitter-heex")
       (html "https://github.com/tree-sitter/tree-sitter-html")
       (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
       (json "https://github.com/tree-sitter/tree-sitter-json")
       (make "https://github.com/alemuller/tree-sitter-make")
       (markdown "https://github.com/ikatyang/tree-sitter-markdown")
       (python "https://github.com/tree-sitter/tree-sitter-python")
       (toml "https://github.com/tree-sitter/tree-sitter-toml")
       (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
       (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
       (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

(use-package diff-hl
  :straight t
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (dired-mode . dired-hide-details-mode))
  :config
  (add-hook 'after-save-hook 'diff-hl-update))

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
