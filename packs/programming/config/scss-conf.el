(require 'scss-mode)

(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(defun scss-mode-changes ()
  (setq scss-compile-at-save nil))

(add-hook 'scss-mode-hook 'scss-mode-changes)
