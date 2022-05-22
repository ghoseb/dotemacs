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
  :mode ("\\.org\\'" . org-mode)
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


(use-package centaur-tabs
  :straight t
  :commands (centaur-tabs-mode centaur-tabs-local-mode)
  :hook
  ((eshell-mode
    help-mode
    helpful-mode
    dired-mode
    occur-mode
    cider-repl-mode
    which-key-mode
    shell-mode)
   . centaur-tabs-local-mode)
  :config
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-bar 'left)
  (setq centaur-tabs-close-button "×")
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-cycle-scope 'tabs)
  (setq centaur-tabs-enable-ido-completion nil)
  (setq centaur-tabs-show-new-tab-button nil)
  (setq centaur-tabs-plain-icons t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-change-fonts bg/variable-pitch-font bg/default-font-size)
  (defun centaur-tabs-buffer-groups ()
     (list
      (cond
       ((not (eq (file-remote-p (buffer-file-name)) nil))
        "Remote")
       ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
        "Term")
       ((or (string-equal "*" (substring (buffer-name) 0 1))
            (memq major-mode '(magit-process-mode
                               magit-status-mode
                               magit-diff-mode
                               magit-log-mode
                               magit-file-mode
                               magit-blob-mode
                               magit-blame-mode)))
        "Emacs")
       ((derived-mode-p 'prog-mode)
        "Editing")
       ((derived-mode-p 'dired-mode)
        "Dired")
       ((memq major-mode '(helpful-mode
                           help-mode))
        "Help")
       ((memq major-mode '(org-mode
                           org-agenda-clockreport-mode
                           org-src-mode
                           org-agenda-mode
                           org-beamer-mode
                           org-indent-mode
                           org-bullets-mode
                           org-cdlatex-mode
                           org-agenda-log-mode
                           diary-mode))
        "OrgMode")
       (t
        (centaur-tabs-get-group-name (current-buffer))))))

  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*Async-native*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*which-key*" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*mybuf" name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name))))))
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-M-<prior>" . centaur-tabs-backward-group)
  ("C-M-<next>" . centaur-tabs-forward-group))

(use-package emacs
  :bind
  ("C-c C-w" . #'world-clock)
  :custom
  (world-clock-list
   '(("Asia/Calcutta" "Pune")
     ("America/Los_Angeles" "San Francisco")
     ("America/New_York" "New York")
     ("Etc/UTC" "UTC"))
   (world-clock-time-format "%a, %d %b %I:%M %p %Z")))

(use-package crux
  :straight t
  :defer t
  :bind
  ([remap kill-whole-line] . #'crux-kill-whole-line)
  ("s-r" . #'crux-recentf-find-file)
  ([(shift return)] . #'crux-smart-open-line)
  ([remap move-beginning-of-line] . #'crux-move-beginning-of-line)
  ("C-8" . #'crux-find-user-init-file))

(use-package keycast
  :straight t
  :demand t
  :config
  (defun bg/toggle-keycast()
    (interactive)
    (if (member '("" keycast-mode-line " ") global-mode-string)
        (progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
               (remove-hook 'pre-command-hook 'keycast--update)
               (message "Keycast OFF"))
      (add-to-list 'global-mode-string '("" keycast-mode-line " "))
      (add-hook 'pre-command-hook 'keycast--update t)
      (message "Keycast ON"))))

;;; utils.el ends here
