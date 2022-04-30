;;; utils.el

(defun bg/kill-current-buffer ()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key "\C-xk" 'bg/kill-current-buffer)


(defun bg/set-global-font-size (font-size)
  "Change FONT-SIZE utility function (globally, all windows)."
  (interactive
   (list (read-number "Font size: " (/ bg/default-font-size 10))))
  (set-face-attribute 'default nil :height (* font-size 10)))


(use-package org
  :straight t
  :mode "\\.org\\'"
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
  :after org
  :hook (org-mode . mixed-pitch-mode))


(use-package olivetti
  :straight t
  :after org
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


(use-package shackle
  :straight t
  :blackout
  :init
  (shackle-mode)
  :custom
  (shackle-rules '(("\\*Apropos\\|Help\\|Occur\\|tide-references\\*"
                    :regexp t
                    :same t
                    :select t
                    :inhibit-window-quit t)
                   ("\\*magit" :regexp t :same t :select t :size 0.4)
                   ("\\*shell.*" :regexp t :same t :select t)
                   ("*Messages*" :select nil :other t)
                   ("*Proced*" :select t :same t)
                   ("*Buffer List*" :select t :same t)
                   ("*Messages*" :same nil :other t :select t :inhibit-window-quit t)
                   ;; clojure
                   ("*sesman CIDER browser*" :inhibit-window-quit t :select t :same t)
                   ("\\*cider-repl" :regexp t :same nil :other t)))
  (shackle-default-rule nil))


;; `brew install libtool cmake' beforehand
(use-package vterm
  :straight t
  :defer t)

;;; utils.el ends here
