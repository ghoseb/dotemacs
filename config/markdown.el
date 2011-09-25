(add-lib-path "markdown-mode")

(require 'markdown-mode)

(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
