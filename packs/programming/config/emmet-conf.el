;; https://github.com/smihica/emmet-mode
(require 'emmet-mode)

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; Indent 2 spaces
(add-hook 'emmet-mode-hook
          (lambda () (setq emmet-indentation 2)))

;; Cursor to be positioned between first empty quotes after expanding
(setq emmet-move-cursor-between-quotes t)
