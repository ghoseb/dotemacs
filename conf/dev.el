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
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (setq diff-hl-draw-borders nil)
  :config
  (global-diff-hl-mode))


(use-package rainbow-delimiters
  :defer t
  :straight t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;; use eglot-mode as lsp client because it's a lot less intrusive
(use-package eglot
  :straight t
  :ensure-system-package (clojure-lsp . "brew install clojure-lsp/brew/clojure-lsp-native")
  :defer t
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
  :defer t
  :straight t
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
  :blackout 1
  :config
  (progn
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode +1)
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)))


(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))
