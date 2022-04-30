;;; utils.el

(defun bg/kill-current-buffer ()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key "\C-xk" 'bg/kill-current-buffer)

(use-package org
  :straight t
  :defer t
  :config
  (setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-image-actual-width '(300)))

(use-package org-superstar
  :straight t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq inhibit-compacting-font-caches t) ; to prevent possible slowdown
  (setq org-superstar-special-todo-items t))

(use-package org-appear
  :after org
  :straight t
  :hook (org-mode . org-appear-mode))

(use-package mixed-pitch
  :straight t
  :hook (org-mode . mixed-pitch-mode))

(use-package olivetti
  :straight t
  :blackout "OL"
  :init
  (setq olivetti-body-width .55)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 2)
          (olivetti-mode t))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (text-scale-decrease 2))))
  :bind
  (("C-s-o" . distraction-free)))

;;; utils.el ends here
