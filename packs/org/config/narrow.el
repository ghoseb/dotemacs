;; Narrow/widen in org-mode agenda

;; Force showing the next headline.
(setq org-show-entry-below (quote ((default))))

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-agenda-remove-restriction-lock)
    (widen)
    (org-agenda-remove-restriction-lock)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" 'bh/widen))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (if (equal major-mode 'org-agenda-mode)
        (bh/set-agenda-restriction-lock 4)
      (widen))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F"
                                  'bh/restrict-to-file-or-follow))
          'append)

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (bh/narrow-to-org-subtree))
    (bh/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N"
                                  'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (bh/narrow-up-one-org-level))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U"
                                  'bh/narrow-up-one-level))
          'append)

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin)
           org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
        (bh/narrow-to-org-project))
    (bh/narrow-to-org-project)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P"
                                  'bh/narrow-to-project))
          'append)

(defvar bh/current-view-project nil)

(defun bh/view-next-project ()
  (interactive)
  (unless (marker-position org-agenda-restrict-begin)
    (goto-char (point-min))
    (re-search-forward "^Projects$")
    (setq bh/current-view-project (point)))
  (bh/widen)
  (goto-char bh/current-view-project)
  (forward-visible-line 1)
  (setq bh/current-view-project (point))
  (if (org-get-at-bol 'org-hd-marker)
      (bh/narrow-to-project)
    (message "All projects viewed.")
    (ding)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V"
                                  'bh/view-next-project))
          'append)

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type)))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
                    (org-agenda-set-restriction-lock restriction-type))))))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<"
                                  'bh/set-agenda-restriction-lock))
          'append)
