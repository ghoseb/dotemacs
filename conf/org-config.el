(defun bg/org-setup ()
  "Customize org by setting a bunch of variables."
  (interactive)
  (visual-line-mode +1)
  (auto-fill-mode -1)
  (whitespace-mode -1))

(defun bg/org-font-setup ()
  "Set faces for heading levels."
  (interactive)
  (dolist (face '((org-level-1 . 1.35)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.12)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font bg--variable-pitch-font :weight 'regular :height (cdr face)))

  (set-face-attribute 'org-document-title nil :font bg--variable-pitch-font :weight 'bold :height 1.5)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :straight t
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . bg/org-setup)
  :custom
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))
  (org-ellipsis " ▾")
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  :config
  (bg/org-font-setup))


(use-package org-superstar
  :straight t
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-special-todo-items t)
  ;; '("◉" "○" "●" "○" "●" "○" "●")
  ;; '("⚫" "◆" "◉" "▶" "◇")
  (org-superstar-headline-bullets-list '("⬤" "▶" "◆" "◉" "○" "◇" "✸" "⁖" "✿"))
  (org-hide-leading-stars nil)
  (org-superstar-leading-bullet ?\s)
  :config
  ; to prevent possible slowdown
  (setq inhibit-compacting-font-caches t))


(use-package org-appear
  :after org
  :straight t
  :hook (org-mode . org-appear-mode))


(use-package visual-fill-column
  :straight t
  :custom
  (visual-fill-column-width 115)
  (visual-fill-column-center-text t)
  :hook
  (visual-line-mode . turn-on-visual-fill-column-mode)
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

(provide 'org-config)
;;; org-config.el ends here
