(require 'uniquify)

(eval-after-load 'uniquify
  '(progn
     (setq uniquify-buffer-name-style 'reverse)
     (setq uniquify-separator "/")
     (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
     (setq uniquify-ignore-buffers-re "^\\*")))

(require 'ansi-color)

;;When you visit a file, point goes to the last place where it was when you previously visited
;;Save file is set to live-tmp-dir/places
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat live-tmp-dir "places"))

;use aspell
(setq-default ispell-program-name "aspell")
;;Setup some dictionary languages
(setq ispell-dictionary "british")'
(setq flyspell-default-dictionary "british")

;;enable winner mode for C-c-(<left>|<right>) to navigate the history
;;of buffer changes i.e. undo a split screen
(when (fboundp 'winner-mode)
      (winner-mode 1))

(setq visible-bell nil
      column-number-mode t
      echo-keystrokes 0.02
      redisplay-dont-pause t
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      shift-select-mode nil
      mouse-yank-at-point t
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
      confirm-nonexistent-file-or-buffer nil)

;; UTF-8 everywhere
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

(set-default 'indent-tabs-mode nil)
(auto-compression-mode t)
(show-paren-mode 1)

;;; eldoc mode for elisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(random t) ;; Seed the random-number generator

(setq diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;;remove all trailing whitespace and trailing blank lines before saving the file
(add-hook 'before-save-hook 'whitespace-cleanup)

(setq custom-file (concat live-etc-dir "emacs-custom.el"))
(load custom-file 'noerror)

;;; Avoid iconizing emacs.
(global-unset-key "\C-z")

;;; Jump to a specific line of the current buffer
(global-set-key (kbd "M-g") 'goto-line)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq ffap-machine-p-known 'accept)
(setq ffap-url-regexp nil)

(add-to-list 'auto-mode-alist '("zshrc$" . shell-script-mode))

(global-set-key (kbd "C-w") 'clipboard-kill-region)
(global-set-key (kbd "M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-y") 'clipboard-yank)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; Indentation help
(global-set-key (kbd "C-x ^") 'join-line)

(setq-default tab-width 4
              standard-indent 4
              indent-tabs-mode nil
              show-trailing-whitespace t)

;; real Emacs hackers don't use the arrow keys
(global-unset-key [up])
(global-unset-key [down])
(global-unset-key [left])
(global-unset-key [right])

;; use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])

;; don't need mail
(global-unset-key (kbd "C-x m"))
