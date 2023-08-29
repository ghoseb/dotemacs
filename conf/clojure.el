;;; clojure.el

(use-package clojure-mode
  :blackout ((clojure-mode . "CLJ")
             (clojurec-mode . "CLJC")
             (clojurescript-mode . "CLJS"))
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode))
  :hook ((clojure-mode . subword-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . eldoc-mode))
  :config
  (setq clojure-indent-style 'always-indent))


(use-package cider
  :straight t
  :hook (clojure-mode . cider-mode)
  :bind (("C-c C-l" . cider-repl-clear-buffer))
  :custom
  (nrepl-log-messages t)
  (cider-repl-display-in-current-window t)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-use-clojure-font-lock t)
  (cider-repl-use-content-types t)
  (cider-save-file-on-load t)
  (cider-prompt-for-symbol nil)
  (cider-font-lock-dynamically '(macro core var))
  (nrepl-hide-special-buffers t)
  (cider-repl-buffer-size-limit 100000)
  (cider-overlays-use-font-lock t)
  (cider-dynamic-indentation nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-prompt-function #'cider-repl-prompt-abbreviated)
  (cider-format-code-options '(("indents" ((".*" (("inner" 0)))))))
  (cider-auto-mode nil)
  (cider-prefer-local-resources t)
  :config
  (defun cider-repl-type-for-buffer (&optional buffer)
    "Return the matching connection type (clj or cljs) for BUFFER.
BUFFER defaults to the `current-buffer'.  In cljc buffers return
multi.  This function infers connection type based on the major mode.
For the REPL type use the function `cider-repl-type'."
    (with-current-buffer (or buffer (current-buffer))
      (cond
       ((seq-some #'derived-mode-p '(clojurescript-ts-mode clojurescript-mode)) 'cljs)
       ((seq-some #'derived-mode-p '(clojurec-ts-mode clojurec-mode)) cider-clojurec-eval-destination)
       ((seq-some #'derived-mode-p '(clojure-ts-mode clojure-mode)) 'clj)
       (cider-repl-type))))
  ;; (defun cider--xref-backend () nil)
  (cider-repl-toggle-pretty-printing))


(use-package apheleia
  :straight t
  :hook (prog-mode . apheleia-mode)
  :ensure-system-package cljstyle
  :config
  (setf (alist-get 'cljstyle apheleia-formatters)
        '("cljstyle" "pipe"))
  (add-to-list 'apheleia-mode-alist '(clojure-mode . cljstyle))
  (add-to-list 'apheleia-mode-alist '(clojurec-mode . cljstyle))
  (add-to-list 'apheleia-mode-alist '(clojurescript-mode . cljstyle))
  (apheleia-global-mode t))

(use-package clojure-ts-mode
  :disabled
  :blackout ((clojure-ts-mode . "CLJ[τ]")
             (clojurec-ts-mode . "CLJC[τ]")
             (clojurescript-ts-mode . "CLJS[τ]"))
  :mode (("\\.clj\\'" . clojure-ts-mode)
         ("\\.cljc\\'" . clojurec-ts-mode)
         ("\\.cljs\\'" . clojurescript-ts-mode)
         ("\\.edn\\'" . clojure-ts-mode))
  :custom
  (treesit-extra-load-path '("~/Code/src/tree-sitter-clojure/dist")))
