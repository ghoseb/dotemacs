;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

;; These are config files, not packages: skip checkdoc's package
;; conventions (Commentary/Code sections, provide footer).
((emacs-lisp-mode . ((flycheck-disabled-checkers . (emacs-lisp-checkdoc)))))
