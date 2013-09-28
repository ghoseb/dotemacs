(require 'sml-mode)

(setenv "PATH" (concat "/usr/local/Cellar/smlnj/110.75/libexec/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/local/Cellar/smlnj/110.75/libexec/bin"  exec-path))

;; README
;; To load a file and run sml repl -> Press F1 inside a sml file.
;; To close the currently running sml repl -> Press F2 inside the repl buffer (also closes the buffer window)
;; To restart the currently running sml repl -> Press F12 inside either the repl or inside the sml file (This assumes you have only two windows open, one for the sml file and other is the sml repl. (since when you press F12 inside a file, it presses C-x o to cycle to the other window and then closes that repl.

(fset 'my-ml-run
      [?\C-x ?\C-s ?\C-c ?\C-l return])

(fset 'my-ml-close
   "\C-xky\C-x0")

(fset 'my-ml-restart-from-file
   [?\C-x ?o f2 f1])

(fset 'my-ml-restart-from-repl
   [f2 f1])

(eval-after-load 'sml-mode
  '(progn
    (define-key sml-mode-map (kbd "C-j") 'reindent-then-newline-and-indent)
    (define-key sml-mode-map (kbd "C-c C-s") 'sml-run)
    (define-key sml-mode-map (kbd "<f1>") 'my-ml-run)
    (define-key inferior-sml-mode-map (kbd "<f2>") 'my-ml-close)
    (define-key sml-mode-map (kbd "<f12>") 'my-ml-restart-from-file)
    (define-key inferior-sml-mode-map (kbd "<f12>") 'my-ml-restart-from-repl)))
