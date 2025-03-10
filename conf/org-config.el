(defun bg/org-setup ()
  "Customize org by setting a bunch of variables."
  (interactive)
  (auto-fill-mode -1))


(use-package verb
  :straight t
  :after org
  :commands (verb-send-request-on-point-other-window-stay)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((verb . t))))


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

  (set-face-attribute 'org-document-title nil :font bg--variable-pitch-font :weight 'bold :height 1.75)
  (set-face-attribute 'org-document-info nil :font bg--variable-pitch-font :weight 'normal :height 1.25)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


(use-package org
  :straight t
  :commands (org-mode)
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . bg/org-setup)
  :custom
  (org-startup-folded t)
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
  (bg/org-font-setup)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))


(use-package org-appear
  :after (org)
  :straight t
  :hook (org-mode . org-appear-mode))


(use-package org-modern
  :straight t
  :hook
  (org-mode . org-modern-mode)
  :custom
  (org-modern-star 'replace))


(use-package visual-fill-column
  :straight t
  :commands (visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))


(use-package org-present
  :straight t
  :commands (org-present-mode)
  :init
  (defvar bg--header-line-face-remap-cookie nil "Var to store the face remapping cookie.")
  (defun bg/org-present-start ()
    (visual-fill-column-mode 1)
    (visual-line-mode 1)
    ;; NOTE: Change the header-line face in the local context
    (setq bg--header-line-face-remap-cookie
          (face-remap-add-relative 'header-line
                                   :box nil
                                   :height 900
                                   :background (ef-themes-get-color-value 'bg-main)))
    (setq header-line-format " "))

  (defun bg/org-present-end ()
    (visual-fill-column-mode 0)
    (visual-line-mode 0)
    (setq header-line-format nil)
    ;; NOTE: Remove face remapping if we have a cookie
    (when bg--header-line-face-remap-cookie
      (face-remap-remove-relative bg--header-line-face-remap-cookie)))

  (defun bg/org-present-prepare-slide (buffer-name heading)
    (org-overview)
    (org-show-entry)
    (org-show-children))
  :hook
  (org-present-mode . bg/org-present-start)
  (org-present-mode-quit . bg/org-present-end)
  :config
  (add-hook 'org-present-after-navigate-functions 'bg/org-present-prepare-slide))


(provide 'org-config)
;;; org-config.el ends here
