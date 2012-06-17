;; Basic org-mode settings.

;; Remove the indentation in the org-file but display it as if it was
;; indented while you are working on the org file buffer.
(setq org-startup-indented t)

;; hides single blank lines and exposes the rest so I can clean them
;; up. A single blank line between headings sometimes reads nicer… so I
;; allow one blank line only.
(setq org-cycle-separator-lines 1)

;; Org mode can fold (cycle) plain lists. I find this setting useful
;; when I have repeating tasks with lots of sublists with checkboxes. I
;; can fold the completed list entries and focus on what is remaining
;; easily.
(setq org-cycle-include-plain-lists t)

;; Prevent creating blank lines before headings and lists items.
(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item . auto))))

;; Adding new tasks quickly without disturbing the current task content.
(setq org-insert-heading-respect-content nil)

;; Show notes at top of the task
(setq org-reverse-note-order nil)

;; Show the hierarchy of tasks above the matched entries and also the
;; immediately following sibling task (but not all siblings).
(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))

;; Enable easy access to the beginning and end of headlines.
(setq org-special-ctrl-a/e 'reversed)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

;; The following setting makes RET open a link instead of inserting a new line.
;; This setting is a love-hate relationship for me. When it first
;; came out I immediately turned it off because I wanted to insert new
;; lines in front of my links and RET would open the link instead, which
;; at the time I found extremely annoying. Since then I've retrained my
;; fingers to hit RET at the end of the previous line.
(setq org-return-follows-link t)

;; I'm finding I use org-occur C-c / / a lot when trying to find details
;; in my org-files. The following setting keeps the highlighted results
;; of the search even after modifying the text. This allows me to edit
;; the file without having to reissue the org-occur command to find the
;; other matches in my file.
(setq org-remove-highlights-with-change nil)

;; Set up org-mode to generate unique attachment IDs with org-id-method.
(setq org-id-method (quote uuidgen))

;; Remove ID property of clones of a subtree.
;; When non-nil, clones of a subtree don't inherit the ID property.
;; Otherwise they inherit the ID property with a new unique
;; identifier.
(setq org-clone-delete-id t)

;; Save all org-buffers automatically once every hour.
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; I insert inactive timestamps when working on org-mode files.
;; For remember tasks the timestamp is in the remember template but for
;; regular structure editing I want the timestamp automatically added
;; when I create the headline.
;; I have a function that is run by an org-mode hook to automatically
;; insert the inactive timestamp whenever a headline is created.
(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (org-return)
    (org-cycle)
    (bh/insert-inactive-timestamp)))

(add-hook 'org-insert-heading-hook
          'bh/insert-heading-inactive-timestamp
          'append)
