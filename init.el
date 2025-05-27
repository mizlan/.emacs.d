;; -*- lexical-binding: t; -*-

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "etc/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(use-package no-littering
  :ensure t)

(elpaca-process-queues)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

(defun disciple/modus-themes-custom-set-faces (&rest _)
  (modus-themes-with-colors
    (custom-set-faces
     '(region ((t :extend nil))))
    (custom-set-faces
     `(secondary-selection
       ((t
         :background ,bg-red-nuanced
         :foreground unspecified
         :extend nil))))))

(defun disciple/show-trailing-whitespace ()
  "Enable highlighting of trailing whitespace"
  (interactive)
  (setq show-trailing-whitespace t))

(defun disciple/smooth-scroll (amt)
  "Scroll smoothly"
  (interactive)
  (pixel-scroll-precision-interpolate amt nil 1))
(defun disciple/smooth-scroll-up ()
  (interactive)
  (disciple/smooth-scroll (window-text-height nil t)))
(defun disciple/smooth-scroll-down ()
  (interactive)
  (disciple/smooth-scroll (- (window-text-height nil t))))

(use-package emacs
  :ensure nil
  :bind
  (("C-v" . #'disciple/smooth-scroll-down)
   ("M-v" . #'disciple/smooth-scroll-up))
  :config
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (menu-bar-mode -1)

  (pixel-scroll-precision-mode 1)
  (savehist-mode 1)

  (let ((treesit-path (no-littering-expand-var-file-name "tree-sitter")))
    (setq treesit--install-language-grammar-out-dir-history (list treesit-path))
    (setq treesit-extra-load-path (list treesit-path)))

  (add-hook 'modus-themes-after-load-theme-hook
	    #'disciple/modus-themes-custom-set-faces)

  (add-hook 'prog-mode-hook #'disciple/show-trailing-whitespace)
  (remove-hook 'prog-mode-hook #'disciple/show-trailing-whitespace)
  (add-hook 'text-mode-hook #'disciple/show-trailing-whitespace)

  (set-frame-font "ZetBrains Mono-16" nil t)
  (require-theme 'modus-themes)
  (modus-themes-load-theme 'modus-operandi)

  :custom
  (frame-resize-pixelwise t)
  (inhibit-splash-screen t)
  (backup-directory-alist '(("." . "~/emacsbackups")))
  (dired-auto-revert-buffer t)
  (enable-recursive-minibuffers t)
  (split-width-threshold 120)

  (ring-bell-function 'ignore)
  (search-whitespace-regexp ".*?")
  (show-paren-delay 0)

  (show-trailing-whitespace t)
  (indent-tabs-mode nil)

  (modus-operandi-palette-overrides '((bg-region bg-cyan-nuanced)
                                      (fg-region unspecified)))
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs nil)

  (mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
  (mouse-wheel-progressive-speed nil)
  (scroll-conservatively 101))

(use-package minions
  :ensure t
  :config
  (minions-mode 1))

(use-package kmacro
  :ensure nil
  :bind ("C-c K" . kmacro-end-or-call-macro))

(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 50))

(use-package which-key
  :ensure nil
  :config
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.2))

(use-package goto-last-change
  :ensure t
  :bind ("C-;" . goto-last-change))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1)
  :custom
  (vertico-cycle t))

(use-package consult
  :ensure t
  :bind
  (("C-c i" . #'consult-imenu)
   ("C-c I" . #'consult-outline)
   ("C-c l" . #'consult-line)
   ("C-c ," . #'consult-buffer)
   ("C-c s" . #'consult-ripgrep)
   ("C-c r" . #'consult-recent-file)
   ("C-c p" . #'consult-project-buffer)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package everforest-theme
  :ensure (everforest
           :host github
           :repo "Theory-of-Everything/everforest-emacs"))

(use-package doom-themes
  :ensure t)

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
	 ("M-." . embark-dwim))
  :config
  ;; TODO don't confirm kill-buffer in switch-buffer
  (setq embark-quit-after-action nil))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
  :config
  (global-corfu-mode)
  :custom
  (tab-always-indent 'complete))

(use-package eldoc-box
  :ensure t)

(use-package expand-region
  :ensure t)

(use-package embrace
  :ensure t)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-kill)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("M" . embrace-commander)
   '("n" . meow-search)
   '("o" . er/expand-region)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-line)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-delete)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :ensure t
  :config
  (add-to-list 'meow-mode-state-list '(notmuch-hello-mode . motion))
  (add-to-list 'meow-mode-state-list '(notmuch-search-mode . motion))
  (add-to-list 'meow-mode-state-list '(notmuch-tree-mode . motion))
  (add-to-list 'meow-mode-state-list '(notmuch-show-mode . motion))

  (meow-thing-register 'angle
                       '(pair ("<") (">"))
                       '(pair ("<") (">")))

  (setq meow-char-thing-table
        '((?f . round)
          (?d . square)
          (?s . curly)
          (?a . angle)
          (?r . string)
          (?v . paragraph)
          (?c . line)
          (?x . buffer)))

  (meow-setup)
  (meow-global-mode 1)
  :custom
  (meow-expand-hint-counts
   '((word . 0) (line . 30) (block . 30) (find . 30) (till . 30) (symbol . 0))))

(use-package magit
  :ensure t
  :bind ("C-c b" . magit-blame)
  :config
  (add-to-list 'magit-blame-styles
	       '(margin
		 (margin-width . 32)
		 (margin-format . ("%c %C %f"))
		 (margin-face . magit-blame-margin)
		 (margin-body-face . magit-blame-dimmed)
		 (show-message . t))))

;; declare transient separately to get an up-to-date version
(use-package transient
  :ensure t)

(use-package diff-hl
  :ensure t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode)
  :custom
  (diff-hl-draw-borders nil))

(use-package auctex
  :ensure ( :host github
            :repo "emacsmirror/auctex"
            :branch "master"))

(use-package yasnippet
  :ensure t)

(use-package org
  :ensure (org :repo "https://code.tecosaur.net/tec/org-mode.git/"
                :branch "dev")
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   :map org-mode-map
   ("C-c i" . consult-org-heading))
  :hook (org-mode . org-indent-mode)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((python . t)
                                         (R . t))))
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  :custom
  (org-log-done 'time)
  (org-hide-emphasis-markers t)
  (org-directory "~/org/")
  (org-agenda-files '("~/org/"))
  (org-capture-templates
   '(("j" "Journal entry" entry
      (file+headline "journal.org" "Journalbob")
      "* %^t journal entry")))
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation t))

(use-package ox-gfm
  :ensure t)

(use-package ess
  :ensure t)

(use-package org-modern
  :ensure t
  :after org
  :config
  (global-org-modern-mode)
  :custom
  (org-modern-star 'replace))

(use-package olivetti
  :ensure t
  :commands (olivetti-mode)
  :bind ("C-c O" . olivetti-mode)
  :custom
  (olivetti-style 'fancy))

(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1)
  :custom
  (gcmh-verbose t))

(use-package dtrt-indent
  :ensure t)

(use-package apheleia
  :ensure t
  :config
  ;; support match-case
  (setf (alist-get 'black apheleia-formatters)
        '("black" "--target-version" "py310" "-"))
  ;; /opt/homebrew/bin has lower PATH priority than
  ;; /Library/Tex/texbin but we would like to use the latexindent
  ;; installed via homebrew to avoid perl errors
  (setf (alist-get 'latexindent apheleia-formatters)
        '("/opt/homebrew/bin/latexindent" "--logfile=/dev/null")))

(use-package copilot
  :ensure ( :host github
	    :repo "copilot-emacs/copilot.el")
  :bind ( :map copilot-mode-map
          ("C-," . copilot-complete)
          :map copilot-completion-map
	  ("TAB" . copilot-accept-completion-by-word))
  :custom
  (copilot-idle-delay nil))

(use-package markdown-mode
  :ensure t)

(use-package websocket
  :ensure t)

(use-package typst-preview
  :ensure ( :host github
	    :repo "havarddj/typst-preview.el"))

(use-package typst-ts-mode
  :ensure t
  :config
  (with-eval-after-load 'eglot
    (with-eval-after-load 'typst-ts-mode
      (add-to-list 'eglot-server-programs
		   '(typst-ts-mode "tinymist"))))

  (custom-set-faces '(typst-ts-superscript-face ((t :height 1.0))))
  (custom-set-faces '(typst-ts-subscript-face ((t :height 1.0))))
  :custom
  (typst-ts-math-script-display '((raise 0) . (raise 0))))

(use-package tuareg
  :ensure t)

(use-package ocaml-eglot
  :ensure t
  :after tuareg
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))

(use-package proof-general
  :ensure t
  :custom
  (proof-splash-enable nil)
  (proof-three-window-mode-policy 'hybrid))

(use-package opam-switch-mode
  :ensure t
  :hook
  ((coq-mode tuareg-mode) . opam-switch-mode))

;; (use-package merlin
;;   :ensure t
;;   :config
;;   (add-hook 'tuareg-mode-hook #'merlin-mode))

(defun disciple/notmuch ()
  "Show the notmuch inbox"
  (interactive)
  (notmuch-search "tag:inbox not tag:trash"))

(use-package keycast
  :ensure t
  :custom
  (keycast-mode-line-remove-tail-elements nil))

(use-package notmuch
  :ensure nil
  :commands notmuch-search
  :bind
  (("C-c m" . disciple/notmuch)

   :map notmuch-search-mode-map
   ("d" . disciple/notmuch-delete)
   ("." . disciple/notmuch-repeat-tag))

  :config
  ;; Allow multiple address completions in buffer.
  ;;
  ;; "Some bug with notmuch address completion due to having notmuch-address-expand-name
  ;; in the message--old-style-completion-functions variable."
  ;; ref: https://github.com/doomemacs/doomemacs/issues/7747#issuecomment-2265358795
  (defun pop-from-message-completion ()
    (pop message--old-style-completion-functions))
  (advice-add 'message-completion-function :after #'pop-from-message-completion)

  (defun disciple/alter-sendmail-args ()
    "Direct lieer to send via either personal or school email heuristically using the 'From:' field"
    (let ((from (save-restriction
		  (message-narrow-to-headers-or-head)
		  (message-fetch-field "From"))))
      (setq message-sendmail-extra-arguments
            '("send" "--quiet" "-t" "-C" (if (string-match-p "ucla" from)
                                             "~/Mail/School"
                                           "~/Mail/Main")))))
  (add-hook 'message-send-hook #'disciple/alter-sendmail-args)
  :custom
  (user-full-name "Michael Lan")
  (user-mail-address "michaellan202@gmail.com")

  (notmuch-search-line-faces '(("important" . warning)
                               ("unread" . notmuch-search-unread-face)
                               ("flagged" . notmuch-search-flagged-face)))
  (notmuch-search-oldest-first nil)
  ;; NOTE: there are two very similar "send mail" commands in
  ;; sendmail.el and message.el. Only the one in message.el supports
  ;; supplying extra arguments, which is required by lieer.
  ;; lieer is in turn required for our school email since app passwords
  ;; are disallowed by the administrators.
  (message-send-mail-function #'message-send-mail-with-sendmail)
  (notmuch-fcc-dirs nil)
  (sendmail-program (expand-file-name "~/Repositories/lieer/venv/bin/gmi"))
  (message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/Mail/Main")))


(defun disciple/notmuch-delete ()
  "Add deletion tags and remove common tags"
  (interactive)
  (notmuch-search-tag '("-inbox" "-important" "-flagged" "+trash")))

;; credit to inwit, ref: https://www.mail-archive.com/notmuch@notmuchmail.org/msg52527.html
(defun disciple/notmuch-repeat-tag ()
  "Repeat the last tagging operation"
  (interactive)
  (notmuch-search-tag
   (split-string-and-unquote (car notmuch-read-tag-changes-history))))

(defun disciple/sync-mail ()
  "Use lieer to sync mail"
  (interactive)
  (compile "cd ~/Mail && ./syncmail"))

;; Pretty cool to use this in multi-cursor workflows, not sure how
;; much other utility it has...
;;
;; ref: https://stackoverflow.com/a/3035574
(defun disciple/replace-last-sexp ()
  "Evaluate sexp before point and replace it with its output"
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(global-set-key (kbd "C-x C-y") #'disciple/replace-last-sexp)
