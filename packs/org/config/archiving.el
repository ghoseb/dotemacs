;; Configuration for org-mode archiving.

;; The following setting ensures that task states are untouched when
;; they are archived. This makes it possible to archive tasks that are
;; not marked DONE. By default tasks are archived under the heading
;; * Archived Tasks in the archive file.
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    (let ((next-headline
           (save-excursion (or (outline-next-heading) (point-max)))))
      ;; Consider only tasks with done todo headings as archivable candidates
      (if (member (org-get-todo-state) org-done-keywords)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (daynr (string-to-number
                         (format-time-string "%d" (current-time))))
                 (a-month-ago (* 60 60 24 (+ daynr 1)))
                 (last-month
                  (format-time-string "%Y-%m-"
                                      (time-subtract (current-time)
                                               (seconds-to-time a-month-ago))))
                 (this-month (format-time-string "%Y-%m-" (current-time)))
                 (subtree-is-current (save-excursion
                                       (forward-line 1)
                                       (and (< (point) subtree-end)
                                            (re-search-forward
                                             (concat last-month "\\|"
                                                     this-month)
                                             subtree-end t)))))
            (if subtree-is-current
                next-headline ; Has a date in this month or last month, skip it
              nil))  ; available to archive
        (or next-headline (point-max))))))
