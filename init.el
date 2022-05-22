;;; init.el --- GNU/Emacs FTW!

(setq bg/conf-dir (expand-file-name "conf" user-emacs-directory))

;; initialise straight.el
(setf straight-base-dir user-emacs-directory)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(straight-use-package 'use-package)

;; GC Hack
(use-package gcmh
  :straight t
  :demand t
  :config
  (gcmh-mode 1))

;; load other settings files

(defun bg/maybe-load (file)
  (if (file-exists-p (expand-file-name file bg/conf-dir))
      (load-file (expand-file-name file bg/conf-dir))))

(bg/maybe-load "settings.el")
(when (memq system-type '(darwin)) (bg/maybe-load "osx.el"))
(bg/maybe-load "core.el")
(bg/maybe-load "dev.el")
(bg/maybe-load "clojure.el")
(bg/maybe-load "utils.el")
(bg/maybe-load "themes.el")

(defun bg/display-startup-time ()
  "Calculate Emacs startup time."
  (message
   "GNU/Emacs (v%s) ready in %s secs (%d GCs)"
   emacs-version
   (format
    "%.3f"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(setq initial-scratch-message nil)

(add-hook 'emacs-startup-hook #'bg/display-startup-time)

;;; init.el ends here
