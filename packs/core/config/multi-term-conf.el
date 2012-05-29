(require 'multi-term)
;; default shell
(setq multi-term-program "/bin/zsh")

(global-set-key "\C-c\M-t" 'multi-term)
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one
