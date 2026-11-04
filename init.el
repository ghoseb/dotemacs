;;; init.el --- GNU/Emacs FTW!  -*- lexical-binding: t; -*-

(load-file (expand-file-name "utils.el" user-emacs-directory))

(defvar bg--local-dir (expand-file-name ".local/" user-emacs-directory) "The directory for package repo, etc.")
(defvar bg--save-dir (expand-file-name "save/" bg--local-dir) "The directory for savefiles.")

(defvar bg--conf-dir (expand-file-name "conf" user-emacs-directory) "The directory for config files.")

;; Defined by straight.el, use-package and utils.el, which load later;
;; declare them so the byte compiler doesn't warn.
(defvar straight-base-dir)
(defvar straight-repository-branch)
(defvar straight-use-package-by-default)
(defvar straight-cache-autoloads)
(defvar straight-vc-git-default-clone-depth)
(defvar straight-check-for-modifications)
(defvar straight-recipes-emacsmirror-use-mirror)
(defvar use-package-always-defer)
(declare-function straight-use-package "straight")
(declare-function bg/maybe-load "utils")

;; straight.el settings (must be set before bootstrap)
(setq straight-base-dir (expand-file-name "var/" bg--local-dir)
      straight-repository-branch "develop"
      straight-use-package-by-default t
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-recipes-emacsmirror-use-mirror nil
      use-package-always-defer t
      ;; follow symlinks silently during bootstrap; restored below
      vc-follow-symlinks t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq vc-follow-symlinks 'ask) ; restore default

;; install use-package
(straight-use-package 'use-package)

(require 'use-package)
(setq use-package-verbose t)


;; GC Hack
(use-package gcmh
  :straight t
  :demand t
  :config
  (gcmh-mode 1))


(use-package benchmark-init
  :straight t
  :demand t
  :config
  (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

;; load other settings files

(bg/maybe-load "settings.el")
(when (memq system-type '(darwin)) (bg/maybe-load "osx.el"))
(bg/maybe-load "core.el")
(bg/maybe-load "themes.el")
(bg/maybe-load "dev.el")
(bg/maybe-load "clojure.el")
(bg/maybe-load "utils.el")

(bg/maybe-load "org-config.el")




;;; init.el ends here
