;;; geiser.el -- main geiser file

;; Copyright (C) 2009, 2010, 2011, 2012 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.


;; Autoloads and basic setup for geiser.

;;; Locations:

(defconst geiser-elisp-dir (file-name-directory load-file-name)
  "Directory containing Geiser's Elisp files.")

(defconst geiser-scheme-dir (expand-file-name "../scheme/" geiser-elisp-dir)
  "Directory containing Geiser's Scheme files.")

(when (not (member geiser-elisp-dir load-path))
  (add-to-list 'load-path geiser-elisp-dir))


;;; Autoloads:

(autoload 'geiser-version "geiser-version" "Echo Geiser's version." t)

(autoload 'geiser-unload "geiser-reload" "Unload all Geiser code." t)

(autoload 'geiser-reload "geiser-reload" "Reload Geiser code." t)

(autoload 'geiser "geiser-repl"
  "Start a Geiser REPL, or switch to a running one." t)

(autoload 'run-geiser "geiser-repl" "Start a Geiser REPL." t)

(autoload 'geiser-connect "geiser-repl"
  "Start a Geiser REPL connected to a remote server." t)

(autoload 'switch-to-geiser "geiser-repl"
  "Switch to a running one Geiser REPL." t)

(autoload 'run-guile "geiser-guile" "Start a Geiser Guile REPL." t)

(autoload 'switch-to-guile "geiser-guile"
  "Start a Geiser Guile REPL, or switch to a running one." t)

(autoload 'connect-to-guile "geiser-guile"
  "Connect to a remote Geiser Guile REPL." t)

(autoload 'run-racket "geiser-racket" "Start a Geiser Racket REPL." t)

(autoload 'run-gracket "geiser-racket" "Start a Geiser GRacket REPL." t)

(autoload 'switch-to-racket "geiser-racket"
  "Start a Geiser Racket REPL, or switch to a running one." t)

(autoload 'connect-to-racket "geiser-racket"
  "Connect to a remote Geiser Racket REPL." t)

(autoload 'geiser-mode "geiser-mode"
  "Minor mode adding Geiser REPL interaction to Scheme buffers." t)

(autoload 'turn-on-geiser-mode "geiser-mode"
  "Enable Geiser's mode (useful in Scheme buffers)." t)

(autoload 'turn-off-geiser-mode "geiser-mode"
  "Disable Geiser's mode (useful in Scheme buffers)." t)

(autoload 'geiser-mode--maybe-activate "geiser-mode")

;;;###autoload
(mapc (lambda (group)
        (custom-add-load group (symbol-name group))
        (custom-add-load 'geiser (symbol-name group)))
      '(geiser
        geiser-repl
        geiser-autodoc
        geiser-doc
        geiser-debug
        geiser-faces
        geiser-mode
        geiser-guile
        geiser-image
        geiser-racket
        geiser-implementation
        geiser-xref))


;;; Setup:

;;;###autoload
(add-hook 'scheme-mode-hook 'geiser-mode--maybe-activate)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))


(provide 'geiser)
