;; Configuration for taking meeting notes in org-mode.

;; Here is the formatting function. Just highlight the region for the
;; notes and it turns tabs into spaces, and highlights todo items. The
;; resulting notes are in the kill buffer ready to paste to another
;; application.
(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email.
   Take selected region and convert tabs to spaces,
   mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): "
                                  (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>)
                                 " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))

;; Having the same list bullet for every list level makes it hard to
;; read the details when lists are indented more than 3 levels.
;; Org-mode has a way to automatically change the list bullets when you
;; change list levels.
(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-"))))
