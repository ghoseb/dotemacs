;;; core.el

(use-package selectrum
  :straight t
  :init (selectrum-mode +1))

(use-package prescient
  :straight t
  :config (prescient-persist-modcient
  :after selectrum
  :straight t
  :init (selectrum-prescient-mode +1))

(use-package ctrlf
  :straight t
  :init (ctrlf-mode +1))

