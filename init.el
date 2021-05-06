;;; -*- lexical-binding: t; -*-

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)

(load-theme 'tango)

(add-to-list 'default-frame-alist '(font . "Roboto Mono-14:light"))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

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
(straight-use-package 'haskell-mode)
(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(straight-use-package 'projectile)
(straight-use-package 'janet-mode)
(straight-use-package 'keycast)
(straight-use-package 'diminish)

(ivy-mode)
(counsel-mode)
(which-key-mode)

(diminish 'ivy-mode)
(diminish 'which-key-mode)
(diminish 'counsel-mode)

(global-set-key (kbd "C-c r") 'counsel-recentf)

(set-face-attribute 'region nil :background "#bb9")
