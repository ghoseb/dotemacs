;;; core.el

(use-package selectrum
  :straight t
  :init (selectrum-mode +1))

(use-package prescient
  :straight t
  :config (prescient-persist-mode +1))

(use-package selectrum-prescient
  :straight t
  :after selectrum
  :init (selectrum-prescient-mode +1))

(use-package ctrlf
  :straight t
  :config (ctrlf-mode +1))

(use-package blackout
  :straight t
  :demand t)

(blackout 'auto-fill-mode)
(blackout 'eldoc-mode)
(blackout 'emacs-lisp-mode "EL")

(use-package use-package-ensure-system-package
  :straight t)


;; misc settings
(setq ring-bell-function #'ignore)
(setq echo-keystrokes 1e-6)
