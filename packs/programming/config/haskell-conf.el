(live-add-pack-lib "haskell")

(load "haskell-site-file")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook '(lambda () (capitalized-words-mode t)))

(setq haskell-program-name "/usr/local/bin/ghci")
