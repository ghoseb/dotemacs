;; Scratch file for org-mode.

(defun bh/make-org-scratch ()
  "Create a scratch file"
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  "Switch to scratch file"
  (interactive)
  (switch-to-buffer "*scratch*"))
