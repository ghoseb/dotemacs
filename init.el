;;; init.el --- GNU/Emacs FTW!

(load-file (expand-file-name "utils.el" user-emacs-directory))

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

(bg/maybe-load "settings.el")
(when (memq system-type '(darwin)) (bg/maybe-load "osx.el"))
(bg/maybe-load "core.el")
(bg/maybe-load "dev.el")
(bg/maybe-load "clojure.el")
(bg/maybe-load "utils.el")
(bg/maybe-load "themes.el")
(bg/maybe-load "org-config.el")

(add-hook 'emacs-startup-hook #'bg/display-startup-msg)

(add-hook 'emacs-startup-hook (lambda ()
                                (when (display-graphic-p)
                                  (ar/show-welcome-buffer))))

;;; init.el ends here
