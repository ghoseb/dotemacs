;;; utils.el

(use-package org
  :straight t
  :defer t)

(use-package org-superstar
  :straight t
  :after org
  :hook (org-mode . org-superstar-mode))

;;; utils.el ends here
