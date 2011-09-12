;;; Global bindings

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

(global-set-key "\C-c\M-t" 'multi-term)
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

(global-set-key (kbd "<f5>") 'revert-all-buffers)

(global-set-key "\C-xk" 'kill-current-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)

(eval-after-load "magit"
  (progn '(global-set-key (kbd "C-x g") 'magit-status)))
