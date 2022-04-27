;;; osx.el

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

(use-package exec-path-from-shell
  :straight t
  :init (exec-path-from-shell-initialize))

