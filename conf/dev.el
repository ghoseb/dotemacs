;;; dev.el

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package magit
  :straight t
  :defer t
  :init
  (setq git-commit-fill-column 72)
  (setq magit-log-arguments '("--graph" "--decorate" "--color"))
  (setq magit-diff-refine-hunk t))


(use-package rainbow-delimiters
  :defer t
  :straight t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))


