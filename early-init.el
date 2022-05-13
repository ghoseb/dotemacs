;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-

;; let's unset this variable and reset after emacs has started
(defvar bg/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist bg/file-name-handler-alist)))


;; we wanna go straight this time!
(setq package-enable-at-startup nil)
(setq package-quickstart nil)
(setq frame-inhibit-implied-resize t)

;; defer GC to much later to speed up the startup process
(setq gc-cons-threshold most-positive-fixnum
      read-process-output-max 16777216
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; restore after startup
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

;; prevent the glimpse of unstyled UI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; initial starting position
(push '(height . 60) default-frame-alist)
(push '(width . 120) default-frame-alist)
(push '(left . 60) default-frame-alist)
(push '(top . 100) default-frame-alist)


;; disable all GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)

(setq ring-bell-function #'ignore)
(setq echo-keystrokes 1e-6)

;; prevent unwanted runtime package builds
(setq comp-deferred-compilation nil)
(setq native-comp-async-report-warnings-errors nil)

;;; early-init.el ends here
