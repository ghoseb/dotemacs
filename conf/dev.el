;;; dev.el

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package magit
  :straight t
  :defer t
  :init
  (setq git-commit-fill-column 72)
  (setq magit-log-arguments '("--graph" "--decorate" "--color"))
  (setq magit-diff-refine-hunk t))


(use-package git-timemachine
  :after magit
  :straight t
  :defer t)


(use-package diff-hl
  :straight t
  :after magit
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (setq diff-hl-draw-borders nil)
  :config
  (global-diff-hl-mode))


(use-package hl-todo
  :straight t
  :defer t
  :init
  (global-hl-todo-mode 1)
  :custom
  (hl-todo-keyword-faces '(("TODO"   . "#BF616A")
                           ("FIXME"  . "#EBCB8B")
                           ("DEBUG"  . "#B48EAD")
                           ("GOTCHA" . "#D08770")
                           ("XXX"   . "#81A1C1"))))


(use-package rainbow-delimiters
  :defer t
  :straight t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))


;; use eglot-mode as lsp client because it's a lot less intrusive
(use-package eglot
  :straight t
  :defer t
  :ensure-system-package (clojure-lsp . "brew install clojure-lsp/brew/clojure-lsp-native")
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c s" . eglot-shutdown))
  :hook
  (clojure-mode . eglot-ensure)
  :custom
  ;; don't need these features as they are provided from elsewhere
  (eglot-ignored-server-capabilities '(:hoverProvider
                                       :documentOnTypeFormattingProvider
                                       :executeCommandProvider)))


(use-package markdown-mode
  :straight t
  :blackout "μ "
  :ensure-system-package multimarkdown
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(use-package json-mode :straight t :defer t)
(use-package yaml-mode :straight t :defer t)


(use-package projectile
  :straight t
  :defer t
  :blackout 1
  :bind
  (:map projectile-mode-map
   ("C-c p" . projectile-command-map))
  :config
  (progn
    (projectile-mode +1)
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)))


(use-package treemacs-projectile
  :after (treemacs projectile)
  :straight t
  :after (treemacs projectile))


(use-package rg
  :straight t
  :commands (rg-menu rg-dwim)
  :ensure-system-package (rg . ripgrep)
  :bind (("C-c s" . rg-menu)
         ("C-c d" . rg-dwim))
  :config
  (rg-enable-default-bindings))


(use-package python
  :straight t
  :mode ("\\.py\\'" . python-mode)
  :blackout "Π"
  :config
  (setq python-shell-interpreter "python3")
  (setq  python-indent-offset 4)
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil))


(use-package smartparens
  :straight t
  :blackout t
  :defer t
  :hook ((prog-mode . smartparens-mode))
  :bind
  (:map smartparens-mode-map
        ("M-(" . #'sp-wrap-round)
        ("M-{" . #'sp-wrap-curly)
        ("M-[" . #'sp-wrap-square))
  :config
  (progn
    (setq sp-base-key-bindings 'paredit)
    (setq sp-autoskip-closing-pair 'always)
    (setq sp-hybrid-kill-entire-symbol nil)
    (sp-use-paredit-bindings)
    (sp-pair "'" nil :unless '(sp-point-after-word-p))
    (sp-local-pair 'emacs-lisp-mode "`" "'")
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'clojure-mode "'" nil :actions nil)
    (sp-local-pair 'cider-mode "'" nil :actions nil)
    (sp-local-pair 'cider-repl-mode "'" nil :actions nil)))


(use-package sql-indent
  :straight t
  :mode ("\\.sql\\'" . sqlind-minor-mode))


(use-package csv-mode
  :straight t
  :blackout "CSV"
  :mode ("\\.csv\\'" . csv-mode)
  :custom (csv-align-max-width 80))
