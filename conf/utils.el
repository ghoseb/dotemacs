;;; utils.el  -*- lexical-binding: t; -*-

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


;; Ghostel: terminal emulator powered by libghostty-vt (the Ghostty VT engine).
;; The native module is ALWAYS built locally from source with `zig' (0.17.x)
;; -- never downloaded -- and the compiled dylib lands in the dotemacs var dir
;; so straight/package rebuilds never clobber a loaded module.
(use-package ghostel
  :straight t
  :hook (after-init . ghostel-status-indicator-mode)
  :bind (:map ghostel-semi-char-mode-map
              ("C-s" . consult-line)
              ("C-k" . bg/ghostel-send-C-k-and-kill)
              ;; Walk shell history with M-p/M-n like eshell, by sending C-p/C-n.
              ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
              ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl"))))
  :custom
  (ghostel-module-auto-install 'compile)
  (ghostel-module-directory (expand-file-name "var/ghostel/" bg--local-dir))
  :init
  (require 'project)
  (add-to-list 'project-switch-commands
               '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands
               '(ghostel-project-list-buffers "Ghostel buffers") t)
  :config
  (defun bg/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel, like Emacs `C-k'.
Kill to end of line and copy the killed text to the `kill-ring'."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))
  ;; Allow launching Emacs commands (e.g. `magit-status') from the shell.
  (add-to-list 'ghostel-eval-cmds
               '("magit-status-setup-buffer" magit-status-setup-buffer)))

;; Consult integration: pick/create terminals with live preview, add ghostel
;; sources to `consult-buffer'/`consult-bookmark', and match across soft wraps.
(use-package consult-ghostel
  :straight t
  :after ghostel
  :hook (after-init . consult-ghostel-mode)
  :bind (("C-x m" . consult-ghostel)
         :map project-prefix-map
         ("m" . consult-ghostel-project)
         ("M" . ghostel-project-list-buffers)
         :map ghostel-semi-char-mode-map
         ("C-c h" . consult-ghostel-history)))

;; The extensions below ship inside the `ghostel' package (hence `:straight
;; nil'); their global-mode commands are autoloaded, so they stay deferred until
;; `after-init' / `eshell-load' fire.

;; Run all `compile' commands in a ghostel buffer.
(use-package ghostel-compile
  :straight nil
  :hook (after-init . ghostel-compile-global-mode))

;; Replace `async-shell-command' with a ghostel version.
(use-package ghostel-shell-command
  :straight nil
  :hook (after-init . ghostel-shell-command-global-mode))

;; Use ghostel's VT parser for comint output (replaces `ansi-color-process-output').
(use-package ghostel-comint
  :straight nil
  :hook (after-init . ghostel-comint-global-mode))

;; Make `eshell-visual-commands' run in a ghostel buffer.
(use-package ghostel-eshell
  :straight nil
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

;; Add a `ghostel:' Org link type.
(use-package ghostel-org
  :straight nil
  :after (ghostel org)
  :demand t)


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
  (defun centaur-tabs-buffer-groups ()
    (list
     (cond
      ((not (eq (file-remote-p (buffer-file-name)) nil))
       "Remote")
      ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'ghostel-mode)
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

(use-package time-zones
  :bind
  ("C-c C-w" . #'time-zones)
  :commands (time-zones)
  :straight (:host github :repo "xenodium/time-zones"))

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

When called in a elisp program, BEGIN and END are region boundaries.

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
