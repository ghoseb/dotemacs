(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(require 'smartparens)
(dolist (x '(scheme emacs-lisp lisp))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) (lambda () (smartparens-strict-mode 1)))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))
