(live-add-pack-lib "expand-region")

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "<f9>") 'er/mark-outside-quotes)
