;;; dev.el

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package xref
  :straight (:type built-in)
  :custom
  (xref-search-program 'ripgrep))


(use-package magit
  :straight t
  :defer 5
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk t)
  (git-commit-fill-column 72)
  (magit-diff-refine-hunk t)
  (magit-section-highlight-hook nil)
  (magit-define-global-key-bindings nil)
  (magit-log-arguments '("--graph" "--decorate" "--color"))
  :config
  (let ((sans-serif-family (face-attribute 'variable-pitch :family)))
    (set-face-attribute 'magit-diff-file-heading nil :family sans-serif-family :weight 'normal :bold nil)
    (set-face-attribute 'magit-diff-file-heading-highlight nil :family sans-serif-family :weight 'normal :bold nil)
    (set-face-attribute 'magit-section-child-count nil :family sans-serif-family :weight 'normal :bold nil)
    (set-face-attribute 'magit-section-heading nil :family sans-serif-family :bold t)
    (set-face-attribute 'magit-section-highlight nil :family sans-serif-family :bold t))
  :bind
  ("C-x g" . magit-status))


(use-package git-timemachine
  :after magit
  :straight (git-timemachine :type git
                             :host gitlab
                             :repo "pidu/git-timemachine"
                             :fork (:host github
                                    :repo "emacsmirror/git-timemachine"))
  :bind (:map prog-mode-map
              ("C-c g t" . git-timemachine)))


(use-package git-modes
  :straight t
  :mode (("\\.gitattributes\\'" . gitattributes-mode)
         ("\\.gitconfig\\'" . gitconfig-mode)
         ("\\.gitignore\\'" . gitignore-mode)))


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
  :init
  (global-hl-todo-mode 1)
  :custom
  (hl-todo-keyword-faces '(("TODO"   . "#BF616A")
                           ("FIXME"  . "#EBCB8B")
                           ("DEBUG"  . "#B48EAD")
                           ("GOTCHA" . "#D08770")
                           ("XXX"   . "#81A1C1"))))


(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))


(use-package jarchive
  :demand t
  :straight (jarchive :type git :host sourcehut :repo "dannyfreeman/jarchive"))

;; use eglot-mode as lsp client because it's a lot less intrusive
(use-package eglot
  :straight (eglot :fork (:repo "joaotavora/eglot"))
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename))
  :hook
  (clojure-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  :config
  (jarchive-setup)
  (jarchive-patch-eglot)
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref nil)
  (eglot-confirm-server-initiated-edits nil)
  ;; don't need these features as they are provided from elsewhere
  (eglot-ignored-server-capabilities '(:hoverProvider
                                       :documentOnTypeFormattingProvider
                                       :executeCommandProvider))
  (eglot-connect-timeout 120))


(use-package markdown-mode
  :straight t
  :blackout "μ "
  :ensure-system-package (multimarkdown)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(use-package yaml-mode :straight t)


(use-package flycheck
  :straight t
  :config
  (setq-default flycheck-indication-mode 'left-fringe)
  (setq-default flycheck-highlighting-mode 'columns)
  :hook
  ;; limiting its use because for other langs we have lsp
  ((emacs-lisp-mode . flycheck-mode)
   (flycheck-mode . flycheck-set-indication-mode)))


(defcustom bg--project-root-markers
  '("project.clj" "shadow-cljs.edn" ".git"
    "Cargo.toml" "compile_commands.json" "compile_flags.txt" "deps.edn")
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)


(defun bg/project-root-p (path)
  "Check if the current PATH has any of the project root markers."
  (catch 'found
    (dolist (marker bg--project-root-markers)
      (when (file-exists-p (concat path marker))
        (throw 'found marker)))))


(defun bg/project-find-root (path)
  "Search up the PATH for `bg--project-root-markers'."
  (when-let ((root (locate-dominating-file path #'bg/project-root-p)))
    (cons 'transient (expand-file-name root))))


(use-package project
  :straight 'gnu-elpa-mirror
  :demand t
  :commands (project-root project-current)
  :bind (:map project-prefix-map ("m" . magit-project-status))
  :config
  (setq project-find-functions (nconc project-find-functions (list #'bg/project-find-root)))
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))


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


(use-package paredit
  :straight t
  :blackout t
  :bind
  (:map paredit-mode-map
        ("M-(" . paredit-wrap-round)
        ("M-{" . paredit-wrap-curly)
        ("{" . paredit-open-curly)
        ("M-[" . paredit-wrap-square)
        ("M-]" . paredit-close-square-and-newline)
        ("C->" . paredit-forward-slurp-sexp)
        ("C-<" . paredit-forward-barf-sexp)
        ("C-M-<" . paredit-backward-slurp-sexp)
        ("C-M->" . paredit-backward-barf-sexp)
        ("RET" . nil)
        ("M-j" . paredit-newline))
  :hook ((clojure-mode . enable-paredit-mode)
         (clojurescript-mode . enable-paredit-mode)
         (clojurec-mode . enable-paredit-mode)
         (cider-repl-mode . enable-paredit-mode)
         (emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode))
  :config
  (show-paren-mode t))


(use-package sql-indent
  :straight t
  :mode ("\\.sql\\'" . sqlind-minor-mode))


(use-package csv-mode
  :straight t
  :blackout "CSV"
  :mode ("\\.csv\\'" . csv-mode)
  :custom (csv-align-max-width 115))


(use-package tempel
  :straight t
  :custom
  (tempel-trigger-prefix "<")
  (tempel-path (expand-file-name "tempel-templates.el" bg--conf-dir))
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook
  (prog-mode . tempel-setup-capf)
  (text-mode . tempel-setup-capf))


(use-package jsonian
  :straight (jsonian :type git
                     :host github
                     :repo "iwahbe/jsonian"
                     :branch "main")
  :mode ("\\.json\\'" . jsonian-mode))


(use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))


(use-package deadgrep
  :straight (deadgrep :type git
                      :host github
                      :repo "Wilfred/deadgrep")
  :bind ("<f5>" . deadgrep))


(use-package spell-fu
  :straight (spell-fu :type git
                      :host codeberg
                      :repo "ideasman42/emacs-spell-fu")
  :init
  (setq ispell-personal-dictionary (expand-file-name "ispell/.pws" bg--save-dir))
  (global-spell-fu-mode)
  :custom
  (spell-fu-faces-include . '(font-lock-doc-face
                              font-lock-comment-face)))

(use-package elisp-slime-nav
  :straight t
  :hook (emacs-lisp-mode . turn-on-elisp-slime-nav-mode))


(use-package devdocs-browser
  :straight t
  :commands (devdocs-browser-install-doc devdocs-browser-open-in))


(use-package tree-sitter
  :straight t
  :ensure-system-package (tree-sitter)
  :hook
  (zig-mode . tree-sitter-hl-mode)
  (js2-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t)
