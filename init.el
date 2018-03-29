;;; init.el - Baishampayan Ghose -*- lexical-binding: t; -*-

;;; Code:

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(setq root-dir (file-name-directory
                (or (buffer-file-name) load-file-name)))

(setq etc-dir (file-name-as-directory (concat root-dir "etc")))

(make-directory etc-dir t)


;; ---------
;; Clean GUI
;; ---------
(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

;; -----------
;; Basic Setup
;; -----------

(setq user-full-name "Baishampayan Ghose")
(setq user-mail-address "b.ghose@gmail.com")

(require 'f)
(eval-when-compile
  (require 'use-package))

(setq default-directory (f-full (getenv "HOME")))

(defun load-local (file)
  (load (f-expand file user-emacs-directory)))


(load-local "defuns")
(load-local "misc")
(when (eq system-type 'darwin)
  (load-local "osx"))

;; --------
;; Packages
;; --------

(use-package diminish
  :ensure t)

(use-package nimbus-theme
  :ensure t)


(use-package auto-compile
  :defer t
  :config (progn
            (auto-compile-on-load-mode 1)
            (auto-compile-on-save-mode -1)
            (setq load-prefer-newer t)
            (setq auto-compile-display-buffer nil)
            (setq auto-compile-mode-line-counter t)))


(use-package misc
  :demand t
  :bind ("M-z" . zap-up-to-char))


(use-package defuns
  :demand t)


(use-package whitespace
  :defer t
  :diminish global-whitespace-mode
  :init (global-whitespace-mode)
  :config (progn
            (setq whitespace-style
                  '(face lines-tail trailing tabs indentation::space))
            (setq whitespace-line-column 78)
            (setq whitespace-global-modes '(not git-commit-mode go-mode))))


(use-package ansi-color
  :init (ansi-color-for-comint-mode-on))


(use-package hl-line
  :config (set-face-background 'hl-line "#073642"))


(use-package dash
  :config (dash-enable-font-lock))


(use-package dired-x
  :defer t)


(use-package ido
  :defer t
  :init (progn (ido-mode 1)
               (ido-everywhere 1))
  :config
  (progn
    (setq ido-case-fold t)
    (setq ido-everywhere t)
    (setq ido-enable-prefix nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-max-prospects 10)
    (setq ido-use-faces nil)
    (add-to-list 'ido-ignore-files "\\.DS_Store")))


(use-package company
  :defer t
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode))


(use-package emacs-lisp-mode
  :defer t
  :diminish t
  :init
  (progn
    (use-package eldoc
      :diminish eldoc-mode
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)))
  :bind (("M-." . find-function-at-point))
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))


(use-package markdown-mode
  :defer t
  :config
  (progn
    (bind-key "M-n" 'open-line-below markdown-mode-map)
    (bind-key "M-p" 'open-line-above markdown-mode-map))
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))


(use-package flx-ido
  :ensure t
  :defer t
  :init (flx-ido-mode 1))


(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :config
  (progn
    (setq flycheck-display-errors-function nil)
    (add-hook 'after-init-hook 'global-flycheck-mode)))


(use-package discover
  :ensure t
  :defer t
  :init (global-discover-mode 1))


(use-package ibuffer
  :defer t
  :config (setq ibuffer-expert t)
  :bind ("C-x C-b" . ibuffer))


(use-package cl-lib-highlight
  :ensure t
  :defer t
  :init (cl-lib-highlight-initialize))


(use-package idomenu
  :defer t
  :bind ("M-i" . idomenu))


(use-package httprepl
  :defer t)


(use-package swoop
  :defer t
  :config (setq swoop-window-split-direction: 'split-window-vertically)
  :bind (("C-o" . swoop)
         ("C-M-o" . swoop-multi)
         ("C-x M-o" . swoop-pcre-regexp)))


(use-package git-gutter
  :defer t
  :ensure t
  :diminish git-gutter-mode
  :init (global-git-gutter-mode +1)
  :bind (("C-x q" . git-gutter:revert-hunk)
         ("C-x x" . git-gutter:popup-diff)
         ("C-c C-s" . git-gutter:stage-hunk)
         ("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)))


;;; Install: godef and gocode first

(use-package go-mode
  :defer t
  :ensure go-mode
  :mode "\\.go\\'"
  :commands (godoc gofmt gofmt-before-save)
  :init
  (progn
    (setq gofmt-command (cond
                         ((executable-find "goimports")
                          "goimports")
                         (t "gofmt")))
    (defun schnouki/maybe-gofmt-before-save ()
      (when (eq major-mode 'go-mode)
        (gofmt-before-save)))
    (defun bg/go-mode-hook ()
      (if (not (string-match "go" compile-command))
          (set (make-local-variable 'compile-command)
               "go build -v && go test -v && go vet")))
    (add-hook 'before-save-hook 'schnouki/maybe-gofmt-before-save)
    (add-hook 'go-mode-hook 'bg/go-mode-hook))
  :config
  (bind-keys :map go-mode-map
             ("C-c C-f" . gofmt)
             ("C-c C-g" . go-goto-imports)
             ("C-c C-k" . godoc)
             ("C-c C-r" . go-remove-unused-imports)
             ("M-." . godef-jump)))


(use-package company-go
  :defer t
  :ensure company-go
  :init (progn
          (use-package company)
          (add-to-list 'company-backends 'company-go)))


(use-package go-eldoc
  :defer t
  :ensure go-eldoc
  :commands go-eldoc-setup
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))


(use-package ibuffer-vc
  :ensure t
  :defer t
  :init (ibuffer-vc-set-filter-groups-by-vc-root))


(use-package rainbow-delimiters
  :ensure t
  :defer t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(use-package rainbow-identifiers
  :ensure t
  :defer t
  :config (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))


(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.yml$" . yaml-mode))


(use-package yasnippet
  :ensure t
  :defer t
  :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :init
  (progn
    (yas-global-mode 1)
    (setq yas-verbosity 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))


(use-package cc-mode
  :defer t
  :config
  (progn
    (add-hook 'c-mode-hook (lambda () (c-set-style "linux")))
    (add-hook 'java-mode-hook (lambda () (c-set-style "linux")))
    (setq tab-width 4)
    (setq c-basic-offset 4)))


(use-package css-mode
  :defer t
  :config (setq css-indent-offset 2))


(use-package js-mode
  :defer t
  :mode ("\\.json$" . js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))))


(use-package js2-mode
  :defer t
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :bind (("C-a" . back-to-indentation-or-beginning-of-line)
         ("C-M-h" . backward-kill-word))
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda ()
                               (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map)))))


(use-package ido-ubiquitous
  :ensure t
  :defer t
  :init (ido-ubiquitous-mode 1))


(use-package nyan-mode
  :ensure t
  :defer t
  :init (nyan-mode 1))


(use-package smex
  :ensure t
  :defer t
  :init (smex-initialize)
  :bind (("M-x" . smex)
         ("C-x M-m" . smex)
         ("C-x C-m" . smex)
         ("M-X" . smex)
         ("C-c C-c M-x" . execute-extended-command)))


(use-package multiple-cursors
  :defer t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))


(use-package popwin
  :defer t
  :config (popwin-mode 1))


(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :init (projectile-global-mode 1)
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'ido)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))


(use-package magit
  :ensure t
  :defer t
  :init (use-package magit-blame :defer t)
  :config
  (progn
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-set-upstream-on-push t)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-restore-window-configuration t)
    (setq magit-last-seen-setup-instructions "1.4.0"))
  :bind (("C-x g" . magit-status)
         ("C-c C-a" . magit-just-amend)))


;; (use-package smartparens
;;   :init
;;   (progn
;;     (use-package smartparens-config)
;;     (use-package smartparens-html)
;;     (smartparens-global-mode 1)
;;     (show-smartparens-global-mode 1))
;;   :config
;;   (progn
;;     (setq smartparens-strict-mode t)
;;     (setq sp-autoescape-string-quote nil)
;;     (setq sp-autoinsert-if-followed-by-word t)
;;     (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
;;     (sp-with-modes '(html-mode sgml-mode)
;;       (sp-local-pair "<" ">"))
;;     (sp-pair "(" ")" :wrap "M-("))
;;   :bind
;;   (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
;;    ("C-M-f" . sp-forward-sexp)
;;    ("C-M-b" . sp-backward-sexp)
;;    ("C-M-n" . sp-up-sexp)
;;    ("C-M-d" . sp-down-sexp)
;;    ("C-M-u" . sp-backward-up-sexp)
;;    ("C-M-p" . sp-backward-down-sexp)
;;    ("C-M-w" . sp-copy-sexp)
;;    ("M-s" . sp-splice-sexp)
;;    ("M-<up>" . sp-splice-sexp)
;;    ("M-r" . sp-splice-sexp-killing-around)
;;    ("C-)" . sp-forward-slurp-sexp)
;;    ("C-}" . sp-forward-barf-sexp)
;;    ("C-(" . sp-backward-slurp-sexp)
;;    ("C-{" . sp-backward-barf-sexp)
;;    ("M-S" . sp-split-sexp)
;;    ("M-J" . sp-join-sexp)
;;    ("C-M-t" . sp-transpose-sexp)))


(use-package ace-jump-mode
  :defer t
  :bind ("C-c SPC" . ace-jump-mode))


(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))


(use-package cua-base
  :init (cua-mode 1)
  :config
  (progn
    (setq cua-enable-cua-keys nil)
    (setq cua-toggle-set-mark nil)))


(use-package uniquify
  :config (progn
            (setq uniquify-buffer-name-style 'reverse)
            (setq uniquify-separator "/")
            (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
            (setq uniquify-ignore-buffers-re "^\\*")))


(use-package saveplace
  :config (setq-default save-place t))


(use-package diff-hl
  :ensure t
  :defer t
  :init (progn
          (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
          (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))
  :config (add-hook 'vc-checkin-hook 'diff-hl-update))


(use-package cider
  :ensure t
  :defer t
  :config (progn
            (use-package clojure-snippets :ensure t :defer t)
            (setq nrepl-hide-special-buffers t)
            (setq cider-prefer-local-resources t)
            (setq cider-show-error-buffer 'only-in-repl)
            (setq cider-stacktrace-default-filters '(tooling dup))
            (setq cider-stacktrace-fill-column 80)
            (setq nrepl-buffer-name-show-port t)
            (setq cider-repl-display-in-current-window t)
            (setq cider-repl-display-help-banner nil)
            (setq cider-prompt-save-file-on-load nil)
            (setq cider-interactive-eval-result-prefix ";; => ")
            (setq cider-repl-history-size 1000)
            (setq cider-repl-history-file (concat etc-dir "cider-history.dat"))
            (add-hook 'cider-repl-mode-hook 'company-mode)
            (add-hook 'cider-mode-hook 'company-mode)
            (add-hook 'cider-repl-mode-hook 'paredit-mode)
            (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode))
  :bind (("C-c C-l" . cider-repl-clear-buffer)))


(use-package eshell
  :defer t
  :bind ("M-e" . eshell)
  :init
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop")))
  :config
  (progn
    (setq eshell-history-size 5000)
    (setq eshell-save-history-on-exit t)))


(use-package smart-mode-line
  :ensure t
  :config (progn
            (setq sml/theme 'automatic)
            (sml/setup))
  :init
  (progn
    (setq-default
     mode-line-format
     '("%e"
       mode-line-front-space
       mode-line-mule-info
       mode-line-client
       mode-line-modified
       mode-line-remote
       mode-line-frame-identification
       mode-line-buffer-identification
       "   "
       mode-line-position
       (vc-mode vc-mode)
       "  "
       mode-line-modes
       mode-line-misc-info
       mode-line-end-spaces))))


(use-package paredit
  :defer t
  :diminish paredit-mode
  :init
  (progn
    (add-hook 'clojure-mode-hook 'enable-paredit-mode)
    (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'ielm-mode-hook 'enable-paredit-mode)
    (add-hook 'json-mode-hook 'enable-paredit-mode))
  :config
  (progn
    (use-package clojure-mode)
    (bind-keys :map clojure-mode-map
               ("M-[" . paredit-wrap-square)
               ("M-{" . paredit-wrap-curly)))
  :bind (("M-)" . paredit-forward-slurp-sexp)
         ("M-(" . paredit-wrap-round)
         ("M-[". paredit-wrap-square)
         ("M-{" . paredit-wrap-curly)))


(use-package clj-refactor
  :defer t
  :ensure t
  :config (progn
            (setq cljr-favor-prefix-notation nil)
            (setq cljr-suppress-middleware-warnings t))
  :init (progn (add-hook 'clojure-mode-hook
                         (lambda ()
                           (cljr-add-keybindings-with-prefix "C-c RET")
                           (clj-refactor-mode 1)))))


(use-package fsharp-mode
  :ensure t
  :defer t
  :mode "\\.fs[iylx]?$")

(use-package hi2
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t
  :init (add-hook 'haskell-mode-hook 'turn-on-hi2)
  :config (progn
            (setq haskell-process-suggest-remove-import-lines t)
            (setq haskell-process-auto-import-loaded-modules t)
            (setq haskell-process-log t)
            (setq haskell-process-type 'cabal-repl)
            (bind-keys :map haskell-mode-map
                       ("C-c C-l" . haskell-process-load-or-reload)
                       ("C-c C-z" . haskell-interactive-switch)
                       ("C-c C-n C-t" . haskell-process-do-type)
                       ("C-c C-n C-i" . haskell-process-do-info)
                       ("C-c C-n C-c" . haskell-process-cabal-build)
                       ("C-c C-n c" . haskell-process-cabal)
                       ("SPC" . haskell-mode-contextual-space))))


(use-package window-number
  :ensure t
  :config (progn
            (window-number-meta-mode 1)
            (window-number-mode 1)))


(use-package flyspell
  :ensure t
  :defer t
  :diminish flyspell-mode
  :init (progn (add-hook 'text-mode-hook 'flyspell-mode)
               (add-hook 'prog-mode-hook 'flyspell-prog-mode)
               (use-package flyspell-lazy
                 :ensure t
                 :init (flyspell-lazy-mode 1)))
  :config (progn
            (setq flyspell-issue-message-flag nil)
            (setq ispell-dictionary "en_GB-ize")
            (when (executable-find "aspell")
              (setq ispell-program-name "aspell")
              (setq ispell-list-command "--list")
              (setq ispell-extra-args '("--sug-mode=ultra"
                                        "--lang=en_GB"
                                        "--run-together"
                                        "--run-together-limit=5"
                                        "--run-together-min=2")))))

(use-package zoom
  :defer t
  :diminish zoom-mode
  :ensure t
  :init (zoom-mode t)
  :config (progn (custom-set-variables
                  '(zoom-size '(0.618 . 0.618))
                  '(zoom-ignored-major-modes '(dired-mode markdown-mode))
                  '(zoom-ignored-buffer-names '("zoom.el" "init.el"))
                  '(zoom-ignored-buffer-name-regexps '("^*calc"))
                  '(zoom-ignore-predicates '((lambda ()
                                               (> (count-lines
                                                   (point-min)
                                                   (point-max))
                                                  20)))))))

(use-package org
  :defer t
  :diminish t
  :config (progn
            (setq org-log-done 'time)
            (setq org-agenda-files (list "~/Dropbox/org/"))
            (setq org-todo-keywords
                  '((sequence "TODO(t)" "FOLLOWUP(f)" "|" "DONE(d)" "DELEGATED(l)" "CANCELLED(c)")))
            (setq org-capture-templates
                  '(("t" "Todo" entry (file+headline "~/Dropbox/org/work.org" "Tasks")
                     "* TODO %?\n%i")
                    ("n" "Note" entry (file+headline "~/Dropbox/org/work.org" "Notes")
                     "* %?\nEntered on %U")
                    ("i" "Idea" entry (file+headline "~/Dropbox/org/work.org" "Ideas")
                     "* %?\n%i\nHatched on %U\n%a")
                    ("m" "Meeting" entry (file+datetree "~/Dropbox/org/meetings.org")
                     "* %?\n  %i" :clock-in t)))
            (bind-keys :map global-map
                       ("C-c l" . org-store-link)
                       ("C-c a" . org-agenda)
                       ("C-c c" . org-capture))))

(use-package elisp-mode
  :diminish t)

;; --------
;; Bindings
;; --------

(bind-key "C-a" 'back-to-indentation-or-beginning-of-line)
(bind-key "C-7" 'comment-or-uncomment-current-line-or-region)
(bind-key "C-6" 'linum-mode)
(bind-key "C-v" 'scroll-up-five)
(bind-key "C-j" 'newline-and-indent)

(bind-key "M-g" 'goto-line)
(bind-key "M-n" 'open-line-below)
(bind-key "M-p" 'open-line-above)
(bind-key "M-+" 'text-scale-increase)
(bind-key "M-_" 'text-scale-decrease)
(bind-key "M-j" 'join-line-or-lines-in-region)
(bind-key "M-v" 'scroll-down-five)
(bind-key "M-k" 'kill-this-buffer)
(bind-key "M-o" 'other-window)
(bind-key "M-1" 'delete-other-windows)
(bind-key "M-2" 'split-window-below)
(bind-key "M-3" 'split-window-right)
(bind-key "M-0" 'delete-window)
(bind-key "M-}" 'next-buffer)
(bind-key "M-{" 'previous-buffer)
(bind-key "M-`" 'other-frame)
(bind-key "M-w" 'kill-region-or-thing-at-point)

(bind-key "C-c g" 'google)
(bind-key "C-c n" 'clean-up-buffer-or-region)
(bind-key "C-c s" 'swap-windows)
(bind-key "C-c r" 'rename-buffer-and-file)
(bind-key "C-c k" 'delete-buffer-and-file)

(bind-key "C-M-h" 'backward-kill-word)

(bind-key
 "C-x C-c"
 (lambda ()
   (interactive)
   (if (y-or-n-p "Quit Emacs? ")
       (save-buffers-kill-emacs))))

(bind-key
 "C-8"
 (lambda ()
   (interactive)
   (find-file user-init-file)))
