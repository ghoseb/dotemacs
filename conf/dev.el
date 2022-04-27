;;; dev.el

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package magit
  :straight t
  :defer t
  :init
  (setq git-commit-fill-column 72)
  (setq magit-log-arguments '("--graph" "--decorate" "--color"))
  (setq magit-diff-refine-hunk t))


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


(use-package lsp-mode
  :straight t
  :defer t
  :ensure-system-package (clojure-lsp . "brew install clojure-lsp/brew/clojure-lsp-native")
  :init (setq lsp-keymap-prefix "C-c l")
  ;; :hook ((clojurescript-mode clojurec-mode clojure-mode) . lsp-deferred)
  :custom
  (lsp-lens-enable t)
  (lsp-signature-auto-activate nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-indentation nil)
  (lsp-enable-completion-at-point nil)
  :commands (lsp lsp-deferred))


(use-package lsp-ui
  :straight t
  :defer t
  :commands lsp-ui-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-ui-doc-position 'bottom)
  :custom
  (lsp-ui-sideline-enable nil))


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
