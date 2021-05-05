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

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package modus-themes
  :ensure
  :init
  (setq modus-themes-bold-constructs nil
        modus-themes-region 'no-extend
        modus-themes-completions 'opinionated)
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi))

(use-package which-key
  :ensure t
  :init
  (setq which-key-show-early-on-C-h t)
  :config
  (which-key-mode))

(use-package ivy
  :ensure t
  :config
  (ivy-mode))

(use-package counsel
  :ensure t
  :config
  (counsel-mode)
  :bind ("C-x C-r" . counsel-recentf))

(use-package magit
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package projectile
  :ensure t)

(use-package haskell-mode
  :ensure t)
