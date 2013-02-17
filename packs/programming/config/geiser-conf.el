(live-add-pack-lib "geiser")

(load "geiser")

(add-hook 'geiser-repl-mode 'enable-paredit-mode)

(eval-after-load 'geiser
  '(progn
     (setq scheme-program-name "/Applications/Racket v5.3.1/bin/racket")
     (setq geiser-racket-binary "/Applications/Racket v5.3.1/bin/racket")
     (setq geiser-active-implementations '(racket))))
