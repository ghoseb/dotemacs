;; -*- lexical-binding: t -*-

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package xref
  :straight (:type built-in)
  :custom
  (xref-search-program 'ripgrep))

(use-package apheleia
  :straight t
  :hook (prog-mode . apheleia-mode)
  ;; FIXME: Clj specific stuff should be moved out of here
  ;; :ensure-system-package cljstyle
  :config
  (setf (alist-get 'cljstyle apheleia-formatters)
        '("cljstyle" "pipe"))
  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(clojure-mode . cljstyle))
  (add-to-list 'apheleia-mode-alist '(clojurec-mode . cljstyle))
  (add-to-list 'apheleia-mode-alist '(clojurescript-mode . cljstyle))
  (apheleia-global-mode t))


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


;; NOTE: Load this after `ef-themes` because we depend on their colors
(use-package hl-todo
  :straight t
  :after (ef-themes)
  :init
  (global-hl-todo-mode 1)
  :config
  (ef-themes-with-colors
    (setq hl-todo-keyword-faces
          `(("DONE" . ,green)
            ("TODO" . ,red)
            ("HOLD" . ,yellow)
            ("OKAY" . ,green-warmer)
            ("NEXT" . ,blue)
            ("NOTE" . ,blue-warmer)
            ("DONT" . ,yellow-warmer)
            ("FAIL" . ,red-warmer)
            ("BUG" . ,red-warmer)
            ("FIXME" . ,red-warmer)
            ("XXX" . ,red-warmer)
            ("DEPRECATED" . ,yellow)
            ("HACK" . ,cyan)))))


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
  (go-mode . eglot-ensure)
  :config
  (jarchive-setup)
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio"))
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref nil)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-sync-connect nil)
  ;; don't need these features as they are provided from elsewhere
  (eglot-ignored-server-capabilities '(:hoverProvider
                                       :documentOnTypeFormattingProvider
                                       :executeCommandProvider))
  (eglot-connect-timeout 120)
  :custom-face
  (eglot-inlay-hint-face  ((t (:inherit shadow :weight semi-light :height 0.8)))))


;; NOTE: This is for making LSP faster.
;; `cargo install emacs-lsp-booster`
(use-package eglot-booster
  :straight (eglot-booster :type git
                           :host github
                           :repo "jdtsmith/eglot-booster"
                           :branch "main")
  :after eglot
  :config (eglot-booster-mode))


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


(use-package tree-sitter-langs
  :straight t)


(use-package python
  :straight (:type built-in)
  :mode ("\\.py\\'" . python-mode)
  :blackout "Π"
  :config
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (add-hook 'python-mode-hook #'tree-sitter-hl-mode))


(use-package python-isort
  :straight t)


(use-package ruff-format
  :straight t)


(use-package pet
  :commands (pet-mode)
  :init
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))
              (when-let ((ruff-executable (pet-executable-find "ruff")))
                (setq-local ruff-format-command ruff-executable)
                (apheleia-mode -1)
                (ruff-format-on-save-mode))
              (when-let ((isort-executable (pet-executable-find "isort")))
                (setq-local python-isort-command isort-executable)
                (python-isort-on-save-mode))
              (pet-eglot-setup)
              (pet-flycheck-setup))))


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
        ("C-(" . paredit-forward-slurp-sexp)
        ("C-{" . paredit-forward-barf-sexp)
        ("C-)" . paredit-backward-slurp-sexp)
        ("C-}" . paredit-backward-barf-sexp)
        ("RET" . nil)
        ("M-;" . nil)
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


(use-package parinfer-rust-mode
  :disabled t
  :hook ((emacs-lisp-mode . parinfer-rust-mode)
         (lisp-interaction-mode . parinfer-rust-mode)
         (eval-expression-minibuffer-setup . parinfer-rust-mode))
  :custom
  (parinfer-rust-library "~/.emacs.d/.local/var/parinfer-rust/libparinfer_rust.dylib"))


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


(use-package zig-mode)

(use-package zig-ts-mode
  :straight (zig-ts-mode
             :type git
             :host codeberg
             :repo "meow_king/zig-ts-mode")
  :blackout "𝒵"
  :mode ("\\.zig\\'" . zig-ts-mode))

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
  :hook (prog-mode . (lambda () (spell-fu-mode -1)))
  :init
  (setq ispell-personal-dictionary (expand-file-name "ispell/.pws" bg--save-dir))
  :config
  (global-spell-fu-mode)
  :custom
  (spell-fu-faces-include . '(font-lock-doc-face
                              font-lock-comment-face)))

(use-package elisp-slime-nav
  :straight t
  :hook (emacs-lisp-mode . turn-on-elisp-slime-nav-mode))


(use-package devdocs
  :straight t
  :commands (devdocs-install devdocs-peruse devdocs-lookup))


(use-package consult-eglot
  :after (eglot)
  :commands (consult-eglot-symbols))


(use-package go-mode
  :after (eglot)
  :mode "\\.go\\'"
  :config
  (setq-default eglot-workspace-configuration
                '((:gopls . ((gofumpt . t))))))


(use-package rust-ts-mode
  :mode "\\.rs\\'")


(use-package transpose-frame
  :straight t
  :commands (transpose-frame flip-frame flop-frame rotate-frame rotate-frame-clockwise rotate-frame-anti-clockwise))


;; Major mode for OCaml programming
(use-package tuareg
  :straight t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)
         ("\\.ml\\'" . tuareg-mode)))

;; Major mode for editing Dune project files
(use-package dune
  :straight t)

;; Merlin provides advanced IDE features
(use-package merlin
  :straight t
  :hook
  (tuareg-mode . merlin-mode)
  :config
  (setq merlin-error-after-save nil))

(use-package merlin-eldoc
  :straight t
  :hook (tuareg-mode . merlin-eldoc-setup))

;; This uses Merlin internally
(use-package flycheck-ocaml
  :straight t
  :hook
  (tuareg-mode . flycheck-ocaml-setup))


(use-package utop
  :straight t
  :hook (tuareg-mode . utop-minor-mode)
  :config
  (setq utop-command "opam exec -- dune utop . -- -emacs")
  :init
  (add-to-list
   'load-path
   (replace-regexp-in-string
    "\n" "/share/emacs/site-lisp"
    (shell-command-to-string "opam var prefix"))))


(use-package gptel
  :straight t
  :init
  ;; Instruct `auth-source` to look into ~/.emacs.d/ for secrets
  (add-to-list 'auth-sources (expand-file-name "var/.authinfo" bg--local-dir))
  (defun bg/get-api-key (hostname)
    "Return a function that retrieves the API key for the given HOSTNAME."
    (lambda ()
      (let ((secret (plist-get (car (auth-source-search :host hostname :user "apikey")) :secret)))
        (when secret
          (funcall secret)))))
  :config
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(deepseek-coder-v2 codellama))
  (setq
   gptel-model 'deepseek-ai/DeepSeek-V3
   gptel-backend (gptel-make-openai "TogetherAI"
                   :host "api.together.xyz"
                   :key (bg/get-api-key "together.ai")
                   :stream t
                   :models '(Qwen/Qwen2.5-Coder-32B-Instruct
                             meta-llama/Llama-3.3-70B-Instruct-Turbo
                             deepseek-ai/DeepSeek-V3))))

(use-package flycheck-overlay
  :straight (flycheck-overlay
             :type git
             :host github
             :repo "konrad1977/flycheck-overlay"))
