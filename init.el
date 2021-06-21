;;; -*- lexical-binding: t; -*-

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(setq-default truncate-lines t)

(setq blink-cursor-blinks 100)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-18"))
(add-to-list 'exec-path "/Users/michaellan/.ghcup/bin")

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(defun edit-config ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;; (setq-default mode-line-format
;;            (list
;;             (propertize " %b %* " 'face 'mode-line-buffer-id)
;;             "(%m) "
;;             "%l "))
(setq mode-line-percent-position nil)
(setq line-number-mode nil)

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

(straight-use-package 'better-defaults)
(straight-use-package 'vertico)
(straight-use-package 'marginalia)
(straight-use-package 'consult)
(straight-use-package 'orderless)
(straight-use-package 'ripgrep)
(straight-use-package 'which-key)
(straight-use-package 'company)
(straight-use-package 'magit)
(straight-use-package 'forge)
(straight-use-package 'projectile)
(straight-use-package 'keycast)
(straight-use-package 'diminish)
(straight-use-package 'flycheck)
(straight-use-package 'lsp-mode)
(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(straight-use-package 'haskell-mode)
(straight-use-package 'lsp-haskell)
(straight-use-package 'typescript-mode)
(straight-use-package 'janet-mode)
(straight-use-package 'vimrc-mode)
(straight-use-package 'lua-mode)
(straight-use-package 'dart-mode)
(straight-use-package 'lsp-dart)
(straight-use-package 'zenburn-theme)
(straight-use-package 'modus-themes)
(straight-use-package 'pass)
(straight-use-package 'password-store)
(straight-use-package 'notmuch)

;; (load-theme 'leuven t)
;; (load-theme 'zenburn t)

(setq modus-themes-bold-constructs t)
(setq modus-themes-completions 'moderate)
(setq modus-themes-region 'bg-only)

(load-theme 'modus-operandi t)

(vertico-mode)
(marginalia-mode)
(which-key-mode)
(projectile-mode)
(show-paren-mode)

(diminish 'which-key-mode)
(diminish 'company-mode)
(diminish 'projectile-mode)
(diminish 'evil-commentary-mode)
(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)

(setq magit-auto-revert-mode nil)
(with-eval-after-load 'magit
  (require 'forge))

(setq completion-styles '(orderless))

(savehist-mode)

;; (evil-define-key 'normal 'global (kbd "<leader>gl") #'lsp)
(setq lsp-keymap-prefix "C-c l")

(define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)

;; (evil-set-leader 'normal (kbd "SPC"))

(setq haskell-interactive-popup-errors nil)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq cider-repl-display-help-banner nil)

(load-file "/Users/michaellan/util/ATS2-Postiats/utils/emacs/ats2-mode.el")
(load-file "/Users/michaellan/util/ATS2-Postiats/utils/emacs/flycheck-ats2.el")

(defun compile-and-run-cpp ()
  "Run a cpp file (meant for competitive programming)."
  (interactive)
  (save-buffer)
  (compile (concat "g++ -std=c++17 -Wshadow -Wall -DNOAM_LOCAL "
                   buffer-file-name
                   " -g -fsanitize=address -fsanitize=undefined -D_GLIBCXX_DEBUG && ./a.out < input") t)
  (evil-window-right 1))

(defun compile-and-run-python ()
  (interactive)
  (save-buffer)
  (compile (concat "python3 "
                   buffer-file-name
                   " < input") t)
  (evil-window-right 1))

; from enberg on #emacs
(add-hook 'compilation-finish-functions
  (lambda (buf str)
    (if (null (string-match ".*exited abnormally.*" str))
        ;;no errors, make the compilation window go away in a few seconds
        (progn
          (run-at-time
           "5 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!")))))

(defun clipboard->file (filepath)
  "Write clipboard to FILEPATH."
  (interactive)
  (with-current-buffer
      (or (get-file-buffer filepath)
          (find-file-noselect filepath))
    (erase-buffer)
    (insert (current-kill 0))
    (save-buffer)))

(defun give-me-the-repo (&optional repo)
  "Clone REPO and open dired"
  (interactive)
  (let ((link (or repo
                  (read-string "link: " )))
        (tempdir (make-temp-file "git-thing" 'directory)))
    (shell-command (concat "git clone --depth=1 "
                           (shell-quote-argument link)
                           " "
                           tempdir))
    (dired tempdir)))

(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)

;; Notmuch

(setq notmuch-address-command 'internal)
;; (evil-define-key 'normal 'global (kbd "<leader>om") #'notmuch)
(global-set-key (kbd "C-c m") #'notmuch)
(setq notmuch-hello-sections '(notmuch-hello-insert-alltags))
(setq notmuch-search-oldest-first nil)
(setq notmuch-saved-searches
      '((:name "inbox" :query "tag:inbox" :key "i")
        (:name "unread" :query "tag:unread" :key "u")
        (:name "flagged" :query "tag:flagged" :key "f")
        (:name "sent" :query "tag:sent" :key "t")
        (:name "drafts" :query "tag:draft" :key "d")
        (:name "all mail" :query "*" :key "a")
        (:name "github" :query "tag:github" :key "g")
        (:name "nnn" :query "tag:nnn" :key "n")))

(defun mizlan/notmuch-delete ()
  (interactive)
  (notmuch-search-add-tag '("+deleted")))

;; taken from https://jonathanchu.is/posts/emacs-notmuch-isync-msmtp-setup/
(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/local/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

;; TODO: figure out a way to do this without void symbol
;; (define-key notmuch-search-mode-map (kbd "d") #'mizlan/notmuch-delete)

(defun go-to-eshell ()
  (interactive)
  (switch-to-buffer "*eshell*"))

(global-set-key (kbd "C-c c") #'edit-config)
(global-set-key (kbd "C-,") #'consult-buffer)
(global-set-key (kbd "C-c SPC") #'consult-line)

(defun select-password ()
  "Interactively select a password and copy it to clipboard."
  (interactive)
  (require 'password-store)
  (let ((pw (password-store--completing-read t)))
    (password-store-copy pw)))

(setq tab-always-indent 'complete)
(setq completion-in-region-function #'consult-completion-in-region)
