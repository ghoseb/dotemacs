;; Set up org-mode
;; This set up is based on http://doc.norang.ca/org-mode.html
;; "Org Mode - Organize Your Life In Plain Text!" by Bernt Hansen,
;; with time stamp: 2012-06-06T22:30-0400
(require 'org-install)
(live-load-config-file "basic.el")
(live-load-config-file "agenda.el")
(live-load-config-file "narrow.el")
(live-load-config-file "archiving.el")
(live-load-config-file "diary.el")
(live-load-config-file "meeting_notes.el")
(live-load-config-file "capture.el")
;; Note: Some IDO settings are in refile.el
(live-load-config-file "refile.el")
;; Note: Some keyword colors are set in todo.el
(live-load-config-file "todo.el")
(live-load-config-file "time_clocking.el")
(live-load-config-file "visibility.el")
(live-load-config-file "scratch.el")
(live-load-config-file "babel.el")
(live-load-config-file "publish.el")
(live-load-config-file "speed_commands.el")
(live-load-config-file "key_bindings.el")
(live-load-config-file "cosmetic.el")
