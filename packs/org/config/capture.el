;; Configuration for capturing tasksm, notes, ... in org-mode.

(setq org-directory "~/git/org")
;; To set up with version control using git:
;; mkdir ~/git/org; cd ~/git/org;
;; git init .
;; git add refile.org
;; git commit -m "org-mode notes file"
(setq org-default-notes-file "~/git/org/refile.org")

;; Use C-M-r to start capture mode
(global-set-key (kbd "C-M-r") 'org-capture)
;; Use C-c r to start capture mode when using SSH from eg an Android phone
(global-set-key (kbd "C-c r") 'org-capture)

;; Org protocol is a great way to create capture notes in org-mode from
;; other applications. I use this to create tasks to review interesting
;; web pages I visit in Firefox.
(require 'org-protocol)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls,
;; and org-protocol.
(setq org-capture-templates
      (quote (("t" "todo"
               entry (file "~/git/org/refile.org")
               "* TODO %?\n%U\n%a\n  %i"
               :clock-in t :clock-resume t)
              ("r" "respond"
               entry (file "~/git/org/refile.org")
               "* TODO Respond to %:from on %:subject\n%U\n%a\n"
               :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note"
               entry (file "~/git/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n  %i"
               :clock-in t :clock-resume t)
              ("j" "Journal"
               entry (file+datetree "~/git/org/diary.org")
               "* %?\n%U\n  %i"
               :clock-in t :clock-resume t)
              ("w" "org-protocol"
               entry (file "~/git/org/refile.org")
               "* TODO Review %c\n%U\n  %i"
               :immediate-finish t)
              ("p" "Phone call"
               entry (file "~/git/org/refile.org")
               "* PHONE %? :PHONE:\n%U"
               :clock-in t :clock-resume t)
              ("h" "Habit"
               entry (file "~/git/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n  %i"))))
