(require 'durendal)

(add-hook 'slime-compilation-finished-hook 'durendal-hide-successful-compile)
(add-hook 'sldb-mode-hook 'durendal-dim-sldb-font-lock)
