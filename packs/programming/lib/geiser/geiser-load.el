(setq geiser-elisp-dir (file-name-directory load-file-name))
(add-to-list 'load-path geiser-elisp-dir)

(require 'geiser)

(setq geiser-scheme-dir "/Users/ghoseb/.emacs.d/packs/programming/lib/geiser/scheme")

(provide 'geiser-load)
