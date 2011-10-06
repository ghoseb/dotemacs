(require 'go-mode-load)

(add-hook 'before-save-hook #'gofmt-before-save)
