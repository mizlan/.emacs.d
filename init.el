;;; -*- lexical-binding: t; -*-

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(add-to-list 'default-frame-alist '(font . "Roboto Mono-14"))
(add-to-list 'exec-path "/Users/michaellan/.ghcup/bin")

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(defun edit-config ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key (kbd "C-'") 'imenu)
(global-set-key (kbd "C-,") 'switch-to-buffer)
(global-set-key (kbd "C-c f") 'projectile--find-file)
(global-set-key (kbd "C-c c") 'edit-config)
(global-set-key (kbd "C-c s") 'counsel-rg)

(setq-default mode-line-format
	      (list
	       (propertize " %b %* " 'face 'mode-line-buffer-id)
	       "(%m) "
	       "%l "))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq evil-want-C-u-scroll t)
(setq evil-mode-line-format nil)

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
(straight-use-package 'evil)
(straight-use-package 'evil-commentary)
(straight-use-package 'keycast)
(straight-use-package 'diminish)
(straight-use-package 'company-mode)
(straight-use-package 'flycheck)
(straight-use-package 'lsp-mode)
(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(straight-use-package 'haskell-mode)
(straight-use-package 'lsp-haskell)
(straight-use-package 'typescript-mode)
(straight-use-package 'janet-mode)
(straight-use-package 'vimrc-mode)
(straight-use-package '(emacs-faff-theme
                        :type git
                        :host github
                        :repo "WJCFerguson/emacs-faff-theme"))
(straight-use-package 'tao-theme)
(straight-use-package 'zenburn-theme)
(straight-use-package 'pass)
(straight-use-package 'password-store)
(straight-use-package 'password-store-otp)
(straight-use-package 'ivy-pass)

(load-theme 'zenburn t)

(evil-mode)
(evil-commentary-mode)
(ivy-mode)
(counsel-mode)
(which-key-mode)
(global-company-mode)
(projectile-mode)
(show-paren-mode)

(diminish 'ivy-mode)
(diminish 'counsel-mode)
(diminish 'which-key-mode)
(diminish 'company-mode)
(diminish 'projectile-mode)
(diminish 'evil-commentary-mode)
(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)

(setq lsp-keymap-prefix "C-c l")

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(global-set-key (kbd "C-c r") 'counsel-recentf)

;; (add-hook 'haskell-mode-hook #'lsp)
;; (add-hook 'haskell-literate-mode-hook #'lsp)

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

(load-file "/Users/michaellan/util/ATS2-Postiats/utils/emacs/ats2-mode.el")
(load-file "/Users/michaellan/util/ATS2-Postiats/utils/emacs/flycheck-ats2.el")

(defun compile-and-run-cpp ()
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

(evil-set-leader 'normal (kbd "SPC"))

(evil-define-key 'normal 'c++-mode-map (kbd "<leader>cc") 'compile-and-run-cpp)
(evil-define-key 'normal 'python-mode-map (kbd "<leader>cc") 'compile-and-run-python)

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

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(defun clipboard-set-file-contents ()
  (interactive)
  (with-current-buffer
      (or (get-file-buffer "/Users/michaellan/code/cp/proco-2021/input")
	  (find-file-noselect "/Users/michaellan/code/cp/proco-2021/input"))
    (erase-buffer)
    (insert (current-kill 0))
    (save-buffer)))

(evil-define-key 'normal 'global (kbd "<leader>fp") 'clipboard-set-file-contents)

(toggle-truncate-lines 1)
