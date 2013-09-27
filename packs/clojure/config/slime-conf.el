;;; WILL CONFLICT WITH CL'S SLIME. ENABLE THIS AT YOUR OWN RISK

(live-add-pack-lib "slime")
(require 'slime)
(slime-setup '(slime-repl slime-scratch slime-editing-commands))
(setq slime-protocol-version 'ignore)
(setq slime-net-coding-system 'utf-8-unix)
(add-hook 'slime-repl-mode-hook '(smartparens-strict-mode 1))

(add-hook 'slime-repl-mode-hook (lambda ()
                                  (modify-syntax-entry ?\{ "(}")
                                  (modify-syntax-entry ?\} "){")
                                  (modify-syntax-entry ?\[ "(]")
                                  (modify-syntax-entry ?\] ")[")))

;;ac-slime auto-complete plugin
(live-add-pack-lib "ac-slime")
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'clojure-mode))
