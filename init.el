;;; init.el - Baishampayan Ghose -*- lexical-binding: t; -*-

;; ---------
;; Clean GUI
;; ---------
(setq ns-use-srgb-colorspace t)

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

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(require 'f)
(require 'use-package)

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

(use-package sublime-themes
  :config (load-theme 'brin :no-confirm))


(use-package auto-compile
  :init (progn
          (auto-compile-on-load-mode 1)
          (auto-compile-on-save-mode -1))
  :config (progn
            (setq load-prefer-newer t)
            (setq auto-compile-display-buffer nil)
            (setq auto-compile-mode-line-counter t)))


(use-package misc
  :demand t
  :bind ("M-z" . zap-up-to-char))

(use-package defuns
  :demand t)


(use-package whitespace
  :init (global-whitespace-mode)
  :config (progn
            (setq whitespace-style '(face empty lines-tail trailing))
            (setq whitespace-line-column 80)
            (setq whitespace-global-modes '(not git-commit-mode))))


(use-package ansi-color
  :init (ansi-color-for-comint-mode-on))


(use-package hl-line
  :config (set-face-background 'hl-line "#073642"))


(use-package dash
  :config (dash-enable-font-lock))


(use-package dired-x)


(use-package ido
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


(use-package emacs-lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)))
  :bind (("M-." . find-function-at-point))
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))


(use-package markdown-mode
  :config
  (progn
    (bind-key "M-n" 'open-line-below markdown-mode-map)
    (bind-key "M-p" 'open-line-above markdown-mode-map))
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))

(use-package flx-ido
  :init (flx-ido-mode 1))


(use-package flycheck
  :diminish "Fly"
  :config
  (progn
    (setq flycheck-display-errors-function nil)
    (add-hook 'after-init-hook 'global-flycheck-mode)))


(use-package discover
  :init (global-discover-mode 1))


(use-package ibuffer
  :config (setq ibuffer-expert t)
  :bind ("C-x C-b" . ibuffer))


(use-package cl-lib-highlight
  :init (cl-lib-highlight-initialize))


(use-package idomenu
  :bind ("M-i" . idomenu))


(use-package httprepl)


(use-package ack-and-a-half)


(use-package swoop
  :config (setq swoop-window-split-direction: 'split-window-vertically)
  :bind (("C-o" . swoop)
         ("C-M-o" . swoop-multi)
         ("C-x M-o" . swoop-pcre-regexp)))


(use-package git-gutter
  :diminish "GG"
  :init (global-git-gutter-mode +1))


(use-package ibuffer-vc
  :init (ibuffer-vc-set-filter-groups-by-vc-root))


(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))


(use-package yasnippet
  :diminish '(yas/minor-mode . "Y")
  :init
  (progn
    (yas-global-mode 1)
    (setq yas-verbosity 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))


(use-package cc-mode
  :config
  (progn
    (add-hook 'c-mode-hook (lambda () (c-set-style "linux")))
    (add-hook 'java-mode-hook (lambda () (c-set-style "linux")))
    (setq tab-width 4)
    (setq c-basic-offset 4)))


(use-package css-mode
  :config (setq css-indent-offset 2))


(use-package js-mode
  :mode ("\\.json$" . js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))))


(use-package js2-mode
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
  :init (ido-ubiquitous-mode 1))


(use-package nyan-mode
  :init (nyan-mode 1))


(use-package smex
  :init (smex-initialize)
  :bind (("M-x" . smex)
         ("C-x M-m" . smex)
         ("C-x C-m" . smex)
         ("M-X" . smex)
         ("C-c C-c M-x" . execute-extended-command)))


(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))


(use-package popwin
  :init (popwin-mode 1))


(use-package projectile
  :diminish projectile-mode
  :init (projectile-global-mode 1)
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'ido)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))


(use-package magit
  :init
  (progn
    (use-package magit-blame)
    (bind-key "C-c C-a" 'magit-just-amend magit-mode-map))
  :config
  (progn
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-set-upstream-on-push t)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-restore-window-configuration t))
  :bind ("C-x g" . magit-status))


(use-package smartparens
  :init
  (progn
    (use-package smartparens-config)
    (use-package smartparens-html)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (progn
    (setq smartparens-strict-mode t)
    (setq sp-autoescape-string-quote nil)
    (setq sp-autoinsert-if-followed-by-word t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
    (sp-pair "(" ")" :wrap "M-("))
  :bind
  (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-p" . sp-backward-down-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-s" . sp-splice-sexp)
   ("M-<up>" . sp-splice-sexp)
   ("M-r" . sp-splice-sexp-killing-around)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))


(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))


(use-package expand-region
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
  :init (global-diff-hl-mode)
  :config (add-hook 'vc-checkin-hook 'diff-hl-update))


(use-package company
  :diminish "Cmp"
  :init (add-hook 'after-init-hook 'global-company-mode))


(use-package cider
  :config (progn
            (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
            (setq nrepl-hide-special-buffers t)
            (setq cider-prefer-local-resources t)
            (setq cider-show-error-buffer 'only-in-repl)
            (setq cider-stacktrace-default-filters '(tooling dup))
            (setq cider-stacktrace-fill-column 80)
            (setq nrepl-buffer-name-show-port t)
            (setq cider-repl-display-in-current-window t)
            (setq cider-prompt-save-file-on-load nil)
            (setq cider-interactive-eval-result-prefix ";; => ")
            (setq cider-repl-history-size 1000)
            (add-hook 'cider-repl-mode-hook 'company-mode)
            (add-hook 'cider-mode-hook 'company-mode)
            (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
            (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)))


(use-package eshell
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
