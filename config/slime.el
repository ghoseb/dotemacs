(add-lib-path "slime")
(require 'slime)

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-repl slime-scratch slime-editing-commands))
     (setq slime-protocol-version 'ignore)
     (setq slime-net-coding-system 'utf-8-unix)))
