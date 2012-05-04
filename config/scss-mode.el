(add-lib-path "scss-mode")

(require 'scss-mode)

(defun scss-mode-changes ()
  (setq scss-compile-at-save nil))

(add-hook 'scss-mode-hook 'scss-mode-changes)
