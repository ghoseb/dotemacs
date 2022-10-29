;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-

;; redirect eln cache
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  ".local/var/eln-cache/" user-emacs-directory))))

;; let's unset this variable and reset after emacs has started
(defvar bg/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist bg/file-name-handler-alist)))


;; we wanna go straight this time!
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

;; defer GC to much later to speed up the startup process
(setq gc-cons-threshold most-positive-fixnum
      read-process-output-max 16777216
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; restore after startup
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

;; uncomment to get rid of titlebar
;; (push '(undecorated . t) default-frame-alist)
;; prevent the glimpse of unstyled UI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; initial starting position
(push '(height . 43) default-frame-alist)
(push '(width . 140) default-frame-alist)
(push '(left . 80) default-frame-alist)
(push '(top . 30) default-frame-alist)


;; disable all GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; sane defaults
(setq inhibit-splash-screen t
      use-file-dialog nil
      ring-bell-function #'ignore
      echo-keystrokes 1e-6
      comp-deferred-compilation nil
      native-comp-async-report-warnings-errors nil
      ;; better scrolling
      scroll-step 1
      scroll-conservatively 101
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(1 ((shift) . 5))
      mouse-wheel-follow-mouse t
      ;; lines between the cursor and the edge of the screen
      scroll-margin 3
      ;; wrap lines that are too long.
      truncate-lines nil
      ;; don't resize frames a character at a time, but use pixels
      frame-resize-pixelwise t
      cursor-in-non-selected-windows nil)

;;; early-init.el ends here
