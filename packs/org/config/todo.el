;; Configuration for todo-list in org-mode.

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;; Keep clock times and states in the LOGBOOK drawer to keep my tasks
;; uncluttered.
(setq org-log-done (quote time))
(setq org-log-into-drawer "LOGBOOK")

;; Add a log entry whenever a task moves to any of the following states:
;; - To or out of DONE status
;; - To WAITING status (with a note) or out of WAITING status
;; - To HOLD status
;; - To CANCELLED status (with a note) or out of CANCELLED status
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|"
                        "DONE(d!/!)")
              (sequence "WAITING(w@/!)"   "HOLD(h@/!)" "|"
                        "CANCELLED(c@/!)" "PHONE"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "orange" :weight bold)
              ("NEXT" :foreground "red"    :weight bold)
;              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "dark khaki" :weight bold)
;              ("HOLD" :foreground "magenta" :weight bold)
;              ("CANCELLED" :foreground "forest green" :weight bold)
;              ("PHONE" :foreground "forest green" :weight bold)
)))

;; Fast todo selection allows changing from any task todo state to any
;; other state directly by selecting the appropriate key from the fast
;; todo selection key menu.
(setq org-use-fast-todo-selection t)
;; Change todo states with S-left and S-right skipping all of the normal
;; processing when entering or leaving a todo state. This cycles through
;; the todo states but skips setting timestamps and entering notes which
;; is very convenient when all you want to do is fix up the status of an
;; entry.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; Triggers that automatically assign tags to tasks based on state
;; changes. If a task moves to CANCELLED state then it gets a CANCELLED
;; tag. Moving a CANCELLED task back to TODO removes the CANCELLED tag.
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (widen)
        (org-narrow-to-subtree)
        (org-show-todo-tree nil))
    (widen)
    (org-narrow-to-subtree)
    (org-show-todo-tree nil)))
