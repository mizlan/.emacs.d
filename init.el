;;; -*- lexical-binding: t; -*-

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)

(add-to-list 'default-frame-alist '(font . "Roboto Mono-14"))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; (package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (eval-when-compile
;;   (require 'use-package))

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

(ivy-mode)
(counsel-mode)
(global-set-key (kbd "C-c r") 'counsel-recentf)

;; (use-package modus-themes
;;   :ensure
;;   :init
;;   (setq modus-themes-bold-constructs nil
;;         modus-themes-region 'no-extend
;;         modus-themes-completions 'opinionated)
;;   (modus-themes-load-themes)
;;   :config
;;   (modus-themes-load-operandi))

;; (use-package which-key
;;   :ensure t
;;   :init
;;   (setq which-key-show-early-on-C-h t)
;;   :config
;;   (which-key-mode))

;; (use-package magit
;;   :ensure t)

;; (use-package clojure-mode
;;   :ensure t)

;; (use-package cider
;;   :ensure t)

;; (use-package projectile
;;   :ensure t)

;; (use-package haskell-mode
;;   :ensure t)

;; (use-package janet-mode
;;   :ensure t)
