;;; core config

(setq user-full-name "Baishampayan Ghose")
(setq user-mail-address "bg@infinitelybeta.com")

(set-default-font "DejaVu Sans Mono-10")

(setq-default tab-width 4
              standard-indent 4
              indent-tabs-mode nil)

(require 'smooth-scrolling)
(require 'rainbow-parens)
(require 'rainbow-delimiters)
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

(add-lib-path "scratch-el")
(require 'scratch)

(add-lib-path "tramp/lisp")
(add-to-list 'Info-default-directory-list (concat dotfiles-lib-dir "tramp/info/"))
(require 'tramp)

(add-lib-path "magit")
(require 'magit)

(add-to-list 'auto-mode-alist '("zshrc$" . shell-script-mode))

(setq custom-file (concat dotfiles-etc-dir "emacs-custom.el"))
(load custom-file 'noerror)

;;; frames
(setq initial-frame-alist '((top . 0)
                            (left . 0)
                            (width . 155)
                            (height . 50)))
(setq frame-title-format "%b")
(setq icon-title-format  "%b")
