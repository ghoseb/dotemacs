;;; misc.el --- Misc configuration -*- lexical-binding: t; -*-

;; Configure backups
;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Cursor-type
;; Use a bar cursor when mark is active and a region exists.
(defun th-activate-mark-init ()
  (setq cursor-type 'bar))

(add-hook 'activate-mark-hook 'th-activate-mark-init)

(defun th-deactivate-mark-init ()
  (setq cursor-type 'box))

(add-hook 'deactivate-mark-hook 'th-deactivate-mark-init)

;; Path related stuff
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq dotfiles-etc-dir (concat dotfiles-dir "etc/"))


;; Custom path
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file)


;; Do not pause on redisplay
(setq redisplay-dont-pause t)

;; Kill whole line
(setq kill-whole-line t)

;; Do not show startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Show keystrokes in minibuffer early
(setq echo-keystrokes 0.1)

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "google-chrome")

;; Initial major mode is Emacs Lisp mode
(setq initial-major-mode 'emacs-lisp-mode)

;; Indent with spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Truncate lines
(set-default 'truncate-lines t)

;; Do not blink cursor
(blink-cursor-mode -1)

;; Do not show any tooltips
(tooltip-mode -1)

;; Remove selected region if typing
(pending-delete-mode 1)

;; Allow some commands
(dolist (command '(narrow-to-region downcase-region upcase-region))
  (put command 'disabled nil))

;; Prefer utf8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Set font size
(set-face-attribute 'default nil :height 140)

;; Do not ask for confirmation
(setq confirm-nonexistent-file-or-buffer nil)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Highlight symbol at point
(add-hook 'find-file-hook 'idle-highlight-mode)

(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Do not show annying menu-bar tips
(setq suggest-key-bindings nil)

;; Make default mode Emacs lisp mode
(setq default-major-mode 'emacs-lisp-mode)

;; Show column number in mode line
(column-number-mode 1)

;; Increase GC threshold
(setq gc-cons-threshold 20000000)

;; no bell
(setq ring-bell-function 'ignore)

;; welcome message
(defun user-first-name ()
  (car  (split-string user-full-name)))

(setq welcome-messages
      (list (concat "Hello " (user-first-name) ", somewhere in the world the Sun is shining for you right now.")
            (concat "Hello " (user-first-name) ", it's lovely to see you again. I do hope that you're well.")
            (concat (user-first-name) ", this could be the beginnings of a great hacking session.")
            (concat (user-first-name) ", may the source be with you; the force, aka GNU Emacs is at your command.")
            (concat (user-first-name) ", turn your head towards the Sun and the shadows will fall behind you.")))

(defun welcome-message ()
  (nth (random (length welcome-messages)) welcome-messages))

(setq initial-scratch-message (concat ";;     MM\"\"\"\"\"\"\"\"`M
;;     MM  mmmmmmmM
;;     M`      MMMM 88d8b.d8b. .d8888b. .d8888b. .d8888b.
;;     MM  MMMMMMMM 88''88'`88 88'  `88 88'  `\"\" Y8ooooo.
;;     MM  MMMMMMMM 88  88  88 88.  .88 88.  ...       88
;;     MM        .M dP  dP  dP `88888P8 '88888P' '88888P'
;;     MMMMMMMMMMMM
;;
;;         M\"\"MMMMMMMM M\"\"M M\"\"MMMMM\"\"M MM\"\"\"\"\"\"\"\"`M
;;         M  MMMMMMMM M  M M  MMMMM  M MM  mmmmmmmM
;;         M  MMMMMMMM M  M M  MMMMP  M M`      MMMM
;;         M  MMMMMMMM M  M M  MMMM' .M MM  MMMMMMMM
;;         M  MMMMMMMM M  M M  MMP' .MM MM  MMMMMMMM
;;         M         M M  M M     .dMMM MM        .M
;;         MMMMMMMMMMM MMMM MMMMMMMMMMM MMMMMMMMMMMM
;;
;;           http://github.com/ghoseb/dotemacs
;;
;; " (welcome-message) "

"))


(provide 'misc)
;;; misc.el ends here
