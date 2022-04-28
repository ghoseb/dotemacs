;;; utils.el

(use-package org
  :straight t
  :defer t)

(use-package org-superstar
  :straight t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq inhibit-compacting-font-caches t) ; to prevent possible slowdown
  (setq org-superstar-special-todo-items t))

(use-package mixed-pitch
  :straight t
  :hook
  (org-mode . mixed-pitch-mode))

;;; utils.el ends here
