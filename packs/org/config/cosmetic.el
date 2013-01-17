;; Cosmetic configuration for org-mode

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)
