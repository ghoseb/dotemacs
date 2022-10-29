;;; init.el --- GNU/Emacs FTW!

(defvar bg--local-dir (expand-file-name ".local/" user-emacs-directory) "The directory for package repo, etc.")
(defvar bg--save-dir (expand-file-name "save/" bg--local-dir) "The directory for savefiles.")

(defvar bg--conf-dir (expand-file-name "conf" user-emacs-directory) "The directory for config files.")

;; initialise straight.el
(setf straight-base-dir (expand-file-name "var/" bg--local-dir))
(setf straight-repository-branch "develop")

;; some settings to make straight.el even better
(setq straight-use-package-by-default t
      use-package-always-defer t
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1
      vc-follow-symlinks t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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
  "Try loading elisp FILE if it exists."
  (if (file-exists-p (expand-file-name file bg--conf-dir))
      (load-file (expand-file-name file bg--conf-dir))))

(bg/maybe-load "settings.el")
(when (memq system-type '(darwin)) (bg/maybe-load "osx.el"))
(bg/maybe-load "core.el")
(bg/maybe-load "dev.el")
(bg/maybe-load "clojure.el")
(bg/maybe-load "utils.el")
(bg/maybe-load "themes.el")
(bg/maybe-load "org-config.el")


(defun bg/num-packages-loaded ()
  "Calculate the number of packages loaded."
  (- (length load-path) (length bg--init-load-path)))


(defun bg/startup-time-str ()
  "Return the startup time as a formatted string."
  (format
    "%.3f"
    (float-time
     (time-subtract after-init-time before-init-time))))


(defun bg/display-startup-msg ()
  "Display the startup message."
  (message
   "GNU/Emacs (v%s) ready with %d packages in %s secs (%d GCs)."
   emacs-version
   (bg/num-packages-loaded)
   (bg/startup-time-str)
   gcs-done))

(setq initial-scratch-message nil)

(add-hook 'emacs-startup-hook #'bg/display-startup-msg)

;;; init.el ends here
