;;; Requires ack installed: http://beyondgrep.com
;;; https://github.com/jhelwig/ack-and-a-half
(require 'ack-and-a-half)

;;; Depends on (included):
;;; https://github.com/magnars/s.el
;;; https://github.com/magnars/dash.el
(require 'projectile)

;;; https://github.com/bbatsov/projectile#basic-setup
(projectile-global-mode)

(eval-after-load 'projectile
  '(progn
    (custom-set-variables
     '(projectile-known-projects-file (concat live-etc-dir "projectile-bookmarks.eld")))))
