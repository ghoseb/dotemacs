;;; init.el --- GNU/Emacs FTW!

;; initialise straight.el
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

;; load other settings files
(setq bg/conf-dir (expand-file-name "conf" user-emacs-directory))
(setq bg/custom-file (expand-file-name "emacs-custom.el" bg/conf-dir))

(defun bg/maybe-load (file)
  (if (file-exists-p (expand-file-name file bg/conf-dir))
      (load-file (expand-file-name file bg/conf-dir))))

(bg/maybe-load "settings.el")
(bg/maybe-load "core.el")
(bg/maybe-load "dev.el")
(bg/maybe-load "clojure.el")
(bg/maybe-load "themes.el")
(bg/maybe-load "utils.el")
(bg/maybe-load "osx.el")

(defun bg/display-startup-time ()
  "Calculate Emacs startup time."
  (message
   "GNU/Emacs: %s secs / %d GC"
   (format
    "%.3f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(setq initial-scratch-message (concat ";; " (bg/display-startup-time)))

(add-hook #'after-init-hook #'(lambda ()
                              ;; restore after startup
                              (setq gc-cons-threshold 16777216
                                    gc-cons-percentage 0.1)))


;;; init.el ends here
