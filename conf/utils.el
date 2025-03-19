;;; utils.el

(defun bg/kill-current-buffer ()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key "\C-xk" 'bg/kill-current-buffer)


(defun bg/set-global-font-size (font-size)
  "Change FONT-SIZE utility function (globally, all windows)."
  (interactive
   (list (read-number "Font size: " (/ bg--default-font-size 10))))
  (set-face-attribute 'default nil :height (* font-size 10)))


(use-package vterm
  :straight t
  :when (bound-and-true-p module-file-suffix)
  :ensure-system-package
  ((libtool . "brew install libtool")
   (cmake . "brew install cmake"))
  :bind (:map vterm-mode-map
              ("<insert>" . ignore)
              ("<f2>" . ignore)
              :map project-prefix-map
              ("t" . vterm-project-dir))
  :custom
  (vterm-always-compile-module t)
  (vterm-environment '("VTERM=1" "LC_ALL=en_US.UTF-8"))
  :preface
  (unless (fboundp 'project-prefixed-buffer-name)
    (autoload #'project-prefixed-buffer-name "project"))
  (defun vterm-project-dir (&optional _)
    "Launch vterm in current project.
Opens an existing vterm buffer for a project if present, unless
the prefix argument is supplied."
    (interactive "P")
    (let* ((default-directory (project-root (project-current t)))
           (name (project-prefixed-buffer-name "vterm")))
      (if (and (not current-prefix-arg) (get-buffer name))
          (switch-to-buffer name)
        (funcall-interactively #'vterm name))))
  :init
  (require 'project)
  (add-to-list 'project-switch-commands
               '(vterm-project-dir "vterm") t)
  ;; Remove window if we exit vterm
  (add-hook 'vterm-exit-functions
            (lambda (_ _)
              (let* ((buffer (current-buffer))
                     (window (get-buffer-window buffer)))
                (when (not (one-window-p))
                  (delete-window window))
                (kill-buffer buffer))))

  (defun bg/display-vterm-buffer (buffer alist)
    (let ((window (display-buffer-in-direction
                   buffer '((direction . bottom)
                            (split-height-threshold . 1)
                            (window-height . .33)))))
      (if window
          (select-window window)
        (message "No appropriate window found for displaying VTerm buffer"))))

  (add-to-list 'display-buffer-alist
               '("\\*vterm"
                 bg/display-vterm-buffer)))


(use-package centaur-tabs
  :straight t
  :commands (centaur-tabs-mode centaur-tabs-local-mode)
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-set-bar 'left)
  (x-underline-at-descent-line t)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-close-button "×")
  (centaur-tabs-height 28)
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-enable-ido-completion nil)
  (centaur-tabs-show-new-tab-button nil)
  (centaur-tabs-plain-icons t)
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-change-fonts bg--variable-pitch-font bg--default-font-size)
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
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*Async-native" name)
       (string-prefix-p "*lsp" name)
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
  :bind
  ([remap kill-whole-line] . #'crux-kill-whole-line)
  ("s-r" . #'crux-recentf-find-file)
  ([(shift return)] . #'crux-smart-open-line)
  ([remap move-beginning-of-line] . #'crux-move-beginning-of-line)
  ("C-8" . #'crux-find-user-init-file))

(use-package keycast
  :straight t
  :commands bg/toggle-keycast
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


(use-package benchmark-init
  :straight t
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

(use-package csv-mode
  :straight t
  :mode "\\.csv\\'"
  :hook
  (csv-mode . csv-align-mode)
  :bind
  ("C-c C-h" . csv-header-line))

(use-package zone-select
  :straight (zone-select :type git :host github :repo "kawabata/zone-select"))


(defun xah/title-case-region-or-line (&optional Begin End)
  "Title case text between nearest brackets, or current line or selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, Begin End are region boundaries.

URL `http://xahlee.info/emacs/emacs/elisp_title_case_text.html'
Version: 2017-01-11 2021-03-30 2021-09-19"
  (interactive)
  (let* ((xskipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕")
         (xp0 (point))
         (xp1 (if Begin
                  Begin
                (if (region-active-p)
                    (region-beginning)
                  (progn
                    (skip-chars-backward xskipChars (line-beginning-position)) (point)))))
         (xp2 (if End
                  End
                (if (region-active-p)
                    (region-end)
                  (progn (goto-char xp0)
                         (skip-chars-forward xskipChars (line-end-position)) (point)))))
         (xstrPairs [
                     [" A " " a "]
                     [" An " " an "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ["'T " "'t "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region xp1 xp2)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (xx)
             (goto-char (point-min))
             (while
                 (search-forward (aref xx 0) nil t)
               (replace-match (aref xx 1) t t)))
           xstrPairs))))))

;;; utils.el ends here
