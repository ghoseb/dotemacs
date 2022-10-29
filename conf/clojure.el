;;; clojure.el

(use-package clojure-mode
  :straight t
  :blackout ((clojure-mode . "CLJ")
             (clojurec-mode . "CLJC")
             (clojurescript-mode . "CLJS"))
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode))
  :hook ((clojure-mode . subword-mode)
         (clojure-mode . smartparens-strict-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . eldoc-mode))
  :config
  (setq clojure-indent-style 'always-indent))


(use-package cider
  :straight t
  :after clojure-mode
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
  (defun cider--xref-backend () nil)
  :hook
  (cider-repl-mode . smartparens-mode))


(use-package apheleia
  :straight t
  :after prog-mode
  :ensure-system-package cljstyle
  :config
  (setf (alist-get 'cljstyle apheleia-formatters)
        '("cljstyle" "pipe"))
  (add-to-list 'apheleia-mode-alist '(clojure-mode . cljstyle))
  (apheleia-global-mode t))
