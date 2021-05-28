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
(straight-use-package 'selectrum)
(straight-use-package 'selectrum-prescient)
(straight-use-package 'counsel)
(straight-use-package 'counsel-projectile)
(straight-use-package 'which-key)
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'evil)
(straight-use-package 'evil-commentary)
(straight-use-package 'evil-collection)
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
(straight-use-package 'zenburn-theme)
(straight-use-package 'pass)
(straight-use-package 'password-store)
(straight-use-package 'ivy-pass)
(straight-use-package 'notmuch)

(straight-use-package '(plz
			:type git
			:host github
			:repo "alphapapa/plz.el"))
(straight-use-package '(ement
			:type git
			:host github
			:repo "alphapapa/ement.el"))

(load-theme 'zenburn t)

(evil-mode)
(evil-commentary-mode)
(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)
;; (ivy-mode)
;; (counsel-mode)
;; (counsel-projectile-mode)
(which-key-mode)
(global-company-mode)
(projectile-mode)
(show-paren-mode)
(global-hl-line-mode)

(diminish 'ivy-mode)
(diminish 'counsel-mode)
(diminish 'which-key-mode)
(diminish 'company-mode)
(diminish 'projectile-mode)
(diminish 'evil-commentary-mode)
(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)

(global-set-key (kbd "C-x C-z") #'selectrum-repeat)

(setq lsp-keymap-prefix "C-c l")

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(global-set-key (kbd "C-c r") 'counsel-recentf)

(evil-set-leader 'normal (kbd "SPC"))

(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)

(setq haskell-interactive-popup-errors nil)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(evil-define-operator cider-eval-region-thing (beg end)
  (interactive "<r>")
  (cider-eval-region beg end))
(evil-define-key 'normal clojure-mode-map (kbd "g r") #'cider-eval-region-thing)

(evil-define-operator elisp-eval-region-thing (beg end)
  (interactive "<r>")
  (eval-region beg end))
(evil-define-key 'normal 'emacs-lisp-mode-map (kbd "g r") #'elisp-eval-region-thing)

(evil-define-key 'insert 'company-active-map (kbd "C-n") 'company-select-next-or-abort)
(evil-define-key 'insert 'company-active-map (kbd "C-p") 'company-select-previous-or-abort)

(setq cider-repl-display-help-banner nil)
(evil-define-key 'normal 'clojure-mode-map (kbd "<leader>dc") 'cider-clojuredocs)
(evil-define-key 'normal 'clojure-mode-map (kbd "<leader>dd") 'cider-doc)
(evil-define-key 'normal 'clojure-mode-map (kbd "g d") 'cider-find-var)

(evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)

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

(defun clipboard->file (filepath)
  "Write clipboard to FILEPATH."
  (interactive)
  (with-current-buffer
      (or (get-file-buffer filepath)
	  (find-file-noselect filepath))
    (erase-buffer)
    (insert (current-kill 0))
    (save-buffer)))

(evil-define-key 'normal 'global (kbd "<leader>fp") '(clipboard->file "/Users/michaellan/code/cp/proco-2021/input"))

(set-default 'truncate-lines t)

(defun give-me-the-repo (&optional repo)
  "Give me REPO."
  (interactive)
  (let ((link (or repo
		  (read-string "link: " )))
        (tempdir (make-temp-file "git-thing" 'directory)))
    (shell-command (concat "git clone --depth=1 "
                           (shell-quote-argument link)
                           " "
                           tempdir))
    (dired tempdir)))
