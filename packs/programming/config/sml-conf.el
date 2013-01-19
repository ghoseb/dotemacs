(require 'sml-mode)

(setenv "PATH" (concat "/usr/local/Cellar/smlnj/110.75/libexec/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/local/Cellar/smlnj/110.75/libexec/bin"  exec-path))

(eval-after-load 'sml-mode
  '(define-key sml-mode-map (kbd "C-j") 'reindent-then-newline-and-indent))
