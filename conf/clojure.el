;;; clojure.el

(use-package cider
  :straight t
  :defer t
  :blackout t
  :bind
  (("C-c C-l" . cider-repl-clear-buffer))
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-clojure-font-lock t
        cider-repl-use-content-types t
        cider-save-file-on-load t
        cider-prompt-for-symbol nil
        cider-font-lock-dynamically '(macro core var)
        nrepl-hide-special-buffers t
        cider-repl-buffer-size-limit 100000
        cider-overlays-use-font-lock t
        cider-dynamic-indentation nil
        cider-repl-display-help-banner nil
        cider-repl-prompt-function #'cider-repl-prompt-abbreviated
        cider-format-code-options '(("indents" ((".*" (("inner" 0)))))))
  (cider-repl-toggle-pretty-printing)
  :init
  (add-hook 'cider-repl-mode-hook #'smartparens-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-hook #'cider-company-enable-fuzzy-completion))


(use-package smartparens
  :straight t
  :blackout t
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
    (show-smartparens-global-mode t)))


(use-package clojure-mode
  :defer t
  :straight t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  :config
  (setq clojure-indent-style 'always-indent))


(use-package apheleia
  :straight t
  :ensure-system-package cljstyle
  :config
  (setf (alist-get 'cljstyle apheleia-formatters)
        '("cljstyle" "pipe"))
  (add-to-list 'apheleia-mode-alist '(clojure-mode . cljstyle))
  (apheleia-global-mode t))
