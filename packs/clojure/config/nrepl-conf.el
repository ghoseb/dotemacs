(live-add-pack-lib "nrepl")
(live-add-pack-lib "ac-nrepl")

(require 'nrepl)
(require 'ac-nrepl)
(require 'smartparens)

(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook '(smartparens-strict-mode 1))
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(eval-after-load "nrepl"
  '(progn
     (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
     (custom-set-variables '(nrepl-history-file (concat live-etc-dir "nrepl-history.eld"))
                           '(nrepl-port "4005"))))


;;; indent Clojure code properly
(eval-after-load "clojure-mode"
  '(progn
    (unless (fboundp 'setq-local)
      (defmacro setq-local (var val)
        `(set (make-local-variable ',var) ,val)))
    (defun fix-nrepl-indentation ()
      (setq-local lisp-indent-function 'clojure-indent-function))
    (add-hook 'nrepl-mode-hook 'fix-nrepl-indentation)))


;;; some personal settings. customize at will
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces nil)
(setq nrepl-popup-stacktraces-in-repl t)
