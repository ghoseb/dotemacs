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


