;;; -*- lexical-binding: t; -*-

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)

(load-theme 'tango)

(add-to-list 'default-frame-alist '(font . "Roboto Mono-14:light"))
(add-to-list 'exec-path "/Users/michaellan/.ghcup/bin")

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(defun edit-config ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key (kbd "C-c c") 'edit-config)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'which-key)
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'janet-mode)
(straight-use-package 'keycast)
(straight-use-package 'diminish)
(straight-use-package 'company-mode)
(straight-use-package 'flycheck)
(straight-use-package 'lsp-mode)
(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(straight-use-package 'haskell-mode)
(straight-use-package 'lsp-haskell)

(ivy-mode)
(counsel-mode)
(which-key-mode)

(diminish 'ivy-mode)
(diminish 'which-key-mode)
(diminish 'counsel-mode)

(setq lsp-keymap-prefix "C-c l")

(global-set-key (kbd "C-c r") 'counsel-recentf)

(set-face-attribute 'region nil :background "#bb9")

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;;; Weird workaround

(defvar original-font-size nil)

(defun adjust-font-size (delta)
  (let* ((old-size (face-attribute 'default :height))
         (new-size (max (max delta (- delta)) (min 300 (+ delta old-size)))))
    (setq original-font-size (or original-font-size old-size))
    (set-face-attribute 'default nil :height new-size)
    (message "Font size set to %d (was %d)" (face-attribute 'default :height) old-size)))

(defun zoom-in ()
  (interactive)
  (adjust-font-size +10))

(defun zoom-out ()
  (interactive)
  (adjust-font-size -10))

(defun zoom-reset ()
  (interactive)
  (when original-font-size
    (set-face-attribute 'default nil :height original-font-size)))

;; Zoom settings
(global-set-key (kbd "C-=") 'zoom-in)
(global-set-key (kbd "C--") 'zoom-out)

