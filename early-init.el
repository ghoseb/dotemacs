;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-

;; we wanna go straight this time!
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

;; defer GC to much later to speed up the startup process
(setq gc-cons-threshold most-positive-fixnum
      read-process-output-max 16777216
      gc-cons-percentage 0.6)

;; prevent the glimpse of unstyled UI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; disable all GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)

;; prevent unwanted runtime package builds
(setq comp-deferred-compilation nil)

(add-hook #'after-init-hook
          #'(lambda ()
              ;; restore after startup
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)))

;;; early-init.el ends here
