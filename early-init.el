;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-

;; Disable package.el and use straight.el instead
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

(defvar bg--init-load-path load-path)

;; Emacs.app launched from the Dock/Finder inherits launchd's minimal PATH
;; (/usr/bin:/bin:/usr/sbin:/sbin); the emacs-plus cask can't inject one.
;; Import PATH from a login shell (~/.zprofile), which takes ~16ms.
;; Launches from a terminal already have the full PATH and skip this.
(defun bg--import-login-shell-path ()
  "Set PATH and `exec-path' from a login shell if PATH lacks Homebrew."
  (unless (string-match-p "/opt/homebrew/bin" (or (getenv "PATH") ""))
    (let ((path (with-temp-buffer
                  ;; The marker skips anything the shell's startup files print.
                  (and (eq 0 (call-process (or (getenv "SHELL") "/bin/zsh") nil '(t nil) nil
                                           "-l" "-c" "printf '\\n__PATH__%s' \"$PATH\""))
                       (goto-char (point-min))
                       (search-forward "\n__PATH__" nil t)
                       (buffer-substring (point) (point-max))))))
      (when (and path (string-match-p "/opt/homebrew/bin" path))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory)))))))

(when (eq system-type 'darwin)
  (bg--import-login-shell-path))

;; redirect eln cache
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  ".local/var/eln-cache/" user-emacs-directory))))

;; let's unset this variable and reset after emacs has started
(defvar bg--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist bg--file-name-handler-alist)
            (makunbound 'bg--file-name-handler-alist)))


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
;; get rid of the title bar
(push '(undecorated-round . t) default-frame-alist)

;; initial starting position
(push '(height . 70) default-frame-alist)
(push '(width . 225) default-frame-alist)
(push '(left . 1100) default-frame-alist)
(push '(top . 575) default-frame-alist)

;; NOTE: For screen-sharing
;; (push '(height . 33) default-frame-alist)
;; (push '(width . 130) default-frame-alist)
;; (push '(left . 1087) default-frame-alist)
;; (push '(top . 121) default-frame-alist)


;; disable all GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Settings that must be in place before init runs. General editor
;; settings live in `use-package emacs' in conf/core.el.
(setq inhibit-splash-screen t
      initial-scratch-message nil
      ;; don't load site-start.el (loaded after early-init, before init)
      site-run-file nil
      ;; don't resize frames a character at a time, but use pixels
      frame-resize-pixelwise t
      native-comp-async-report-warnings-errors nil
      ;; Log warnings to *Warnings* but only pop the buffer up for errors.
      warning-minimum-level :error
      debug-on-error t)

;;; early-init.el ends here
