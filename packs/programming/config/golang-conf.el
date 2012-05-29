(live-add-pack-lib "golang")
(require 'go-mode-load)
(require 'go-mode)

(add-hook 'before-save-hook #'gofmt-before-save)
