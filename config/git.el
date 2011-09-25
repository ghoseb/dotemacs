(add-lib-path "mo-git-blame")

(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

(global-set-key (kbd "C-c g c") 'mo-git-blame-current)
(global-set-key (kbd "C-c g f") 'mo-git-blame-file)


(add-lib-path "magit")
(require 'magit)

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (set-face-background 'magit-item-highlight "gray15")
     (set-face-foreground 'magit-diff-hunk-header "orange")
     (set-face-foreground 'magit-branch "gold")))
