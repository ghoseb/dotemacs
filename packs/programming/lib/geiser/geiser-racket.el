;; geiser-racket.el -- geiser support for Racket scheme

;; Copyright (C) 2009, 2010, 2011, 2012 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Apr 25, 2009 21:13



(require 'geiser-edit)
(require 'geiser-doc)
(require 'geiser-eval)
(require 'geiser-image)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)
(require 'geiser)


;;; Customization:

(defgroup geiser-racket nil
  "Customization for Geiser's Racket flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-racket-binary
  (cond ((eq system-type 'windows-nt) "Racket.exe")
        (t "racket"))
  "Name to use to call the racket executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-racket)

(geiser-custom--defcustom geiser-racket-gracket-binary
  (cond ((eq system-type 'windows-nt) "GRacket-text.exe")
        (t "gracket-text"))
  "Name to use to call the gracket executable when starting a REPL.
This executable is used by `run-gracket', and, if
`geiser-racket-use-gracket-p' is set to t, by `run-racket'."
  :type '(choice string (repeat string))
  :group 'geiser-racket)

(geiser-custom--defcustom geiser-racket-collects nil
  "A list of paths to be added to racket's collection directories."
  :type '(repeat file)
  :group 'geiser-racket)

(geiser-custom--defcustom geiser-racket-init-file "~/.racket-geiser"
  "Initialization file with user code for the racket REPL."
  :type 'string
  :group 'geiser-racket)

(geiser-custom--defcustom geiser-racket-use-gracket-p nil
  "Whether to use the gracket binary to start Racket REPLs."
  :type 'boolean
  :group 'geiser-racket)

(geiser-custom--defcustom geiser-racket-extra-keywords
    '("define-syntax-rule" "provide" "require"
      "unless" "when" "with-handlers")
  "Extra keywords highlighted in Racket buffers."
  :type '(repeat string)
  :group 'geiser-racket)


;;; REPL support:

(defsubst geiser-racket--real-binary ()
  (if geiser-racket-use-gracket-p
      geiser-racket-gracket-binary
    geiser-racket-binary))

(defun geiser-racket--binary ()
  (let ((binary (geiser-racket--real-binary)))
    (if (listp binary) (car binary) binary)))

(defun geiser-racket--parameters ()
  "Return a list with all parameters needed to start racket.
This function uses `geiser-racket-init-file' if it exists."
  (let ((init-file (and (stringp geiser-racket-init-file)
                        (expand-file-name geiser-racket-init-file)))
        (binary (geiser-racket--real-binary))
        (rackdir (expand-file-name "racket/" geiser-scheme-dir)))
    `("-i" "-q"
      "-S" ,rackdir
      ,@(apply 'append (mapcar (lambda (p) (list "-S" p))
                               geiser-racket-collects))
      ,@(and (listp binary) (cdr binary))
      ,@(and init-file (file-readable-p init-file) (list "-f" init-file))
      "-f" ,(expand-file-name "geiser/startup.rkt" rackdir))))

(defconst geiser-racket--prompt-regexp "\\(mzscheme\\|racket\\)@[^ ]*?> ")

(defun geiser-racket--startup (remote)
  (if geiser-image-cache-dir
      (geiser-eval--send/wait
       `(:eval (image-cache ,geiser-image-cache-dir) geiser/user))
    (setq geiser-image-cache-dir
          (geiser-eval--send/result '(:eval (image-cache) geiser/user)))))


;;; Remote REPLs

(defun connect-to-racket ()
  "Start a Racket REPL connected to a remote process.

The remote process needs to be running a REPL server started
using start-geiser, a procedure in the geiser/server module."
  (interactive)
  (geiser-connect 'racket))



;;; Evaluation support:

(defun geiser-racket--language ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
         "^\\(?:#lang\\|(module +[^ ]+?\\) +\\([^ ]+?\\|([^)]+)\\) *$" nil t)
        (car (geiser-syntax--read-from-string (match-string-no-properties 1)))
      "#f")))

(defun geiser-racket--enter-command (module)
  (when (stringp module)
    (cond ((zerop (length module)) ",enter #f")
          ((file-name-absolute-p module) (format ",enter %S" module))
          (t (format ",enter %s" module)))))

(defun geiser-racket--geiser-procedure (proc &rest args)
  (case proc
    ((eval compile)
     (format ",geiser-eval %s %s %s"
             (or (car args) "#f")
             (geiser-racket--language)
             (mapconcat 'identity (cdr args) " ")))
    ((load-file compile-file)
     (format ",geiser-eval geiser/main racket (geiser:%s %s)"
             proc (car args)))
    ((no-values) ",geiser-no-values")
    (t (format ",apply geiser:%s (%s)" proc (mapconcat 'identity args " ")))))

(defconst geiser-racket--module-re
  "^(module +\\([^ ]+\\)")

(defun geiser-racket--explicit-module ()
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward geiser-racket--module-re nil t)
         (ignore-errors
           (car (geiser-syntax--read-from-string
                 (match-string-no-properties 1)))))))

(defsubst geiser-racket--implicit-module ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#lang " nil t)
        (buffer-file-name)
      :f)))

(defun geiser-racket--get-module (&optional module)
  (cond ((and (null module) (buffer-file-name)))
        ;; (geiser-racket--explicit-module)
        ((null module) (geiser-racket--implicit-module))
        ((symbolp module) module)
        ((and (stringp module) (file-name-absolute-p module)) module)
        ((stringp module) (make-symbol module))
        (t nil)))

(defun geiser-racket--symbol-begin (module)
  (save-excursion (skip-syntax-backward "^-()>") (point)))

(defun geiser-racket--import-command (module)
  (and (stringp module)
       (not (zerop (length module)))
       (format "(require %s)" module)))

(defun geiser-racket--exit-command ()
  (comint-send-eof)
  (get-buffer-process (current-buffer)))

(defconst geiser-racket--binding-forms
  '("for" "for/list" "for/hash" "for/hasheq" "for/and" "for/or"
    "for/lists" "for/first" "for/last" "for/fold"
    "for:" "for/list:" "for/hash:" "for/hasheq:" "for/and:" "for/or:"
    "for/lists:" "for/first:" "for/last:" "for/fold:"
    "define-syntax-rule"))

(defconst geiser-racket--binding-forms*
  '("for*" "for*/list" "for*/lists" "for*/hash" "for*/hasheq" "for*/and"
    "for*/or" "for*/first" "for*/last" "for*/fold"
    "for*:" "for*/list:" "for*/lists:" "for*/hash:" "for*/hasheq:" "for*/and:"
    "for*/or:" "for*/first:" "for*/last:" "for*/fold:"))

;;; External help

(defsubst geiser-racket--get-help (symbol module)
  (geiser-eval--send/wait
   `(:eval (get-help ',symbol '(:module ,module)) geiser/autodoc)))

(defun geiser-racket--external-help (id module)
  (message "Looking up manual for '%s'..." id)
  (let ((out (geiser-eval--retort-output
              (geiser-racket--get-help id module))))
    (when (and out (string-match " but provided by:\n +\\(.+\\)\n" out))
      (geiser-racket--get-help id (match-string 1 out))))
  (minibuffer-message "%s done" (current-message))
  t)


;;; Error display

(defconst geiser-racket--file-rxs
  '(nil
    "path:\"?\\([^>\"\n]+\\)\"?>"
    "module: \"\\([^>\"\n]+\\)\""))

(defconst geiser-racket--geiser-file-rx
  (format "^%s/?racket/geiser" (regexp-quote geiser-scheme-dir)))

(defun geiser-racket--purge-trace ()
  (save-excursion
    (while (re-search-forward geiser-racket--geiser-file-rx nil t)
      (kill-whole-line))))

(defun geiser-racket--display-error (module key msg)
  (when key
    (insert "Error: ")
    (geiser-doc--insert-button key nil 'racket)
    (newline 2))
  (when msg
    (let ((p (point)))
      (insert msg)
      (let ((end (point)))
        (goto-char p)
        (when key (geiser-racket--purge-trace))
        (mapc 'geiser-edit--buttonize-files geiser-racket--file-rxs)
        (goto-char end)
        (newline))))
  (or key (not (zerop (length msg)))))


;;; Trying to ascertain whether a buffer is mzscheme scheme:

(defun geiser-racket--guess ()
  (or (save-excursion
        (goto-char (point-min))
        (re-search-forward "#lang " nil t))
      (geiser-racket--explicit-module)))


;;; Keywords and syntax

(defun geiser-racket--keywords ()
  (append '(("^#lang\\>" . 0)
            ("\\[\\(else\\)\\>" . 1))
          (when geiser-racket-extra-keywords
            `((,(format "[[(]%s\\>" (regexp-opt geiser-racket-extra-keywords 1))
               . 1)))))

(geiser-syntax--scheme-indent
 (begin0 1)
 (case-lambda: 0)
 (class* defun)
 (compound-unit/sig 0)
 (define: defun)
 (for 1)
 (for* 1)
 (for*/and 1)
 (for*/first 1)
 (for*/fold 2)
 (for*/hash 1)
 (for*/hasheq 1)
 (for*/hasheqv 1)
 (for*/last 1)
 (for*/list 1)
 (for*/lists 2)
 (for*/or 1)
 (for*/product 1)
 (for*/sum 1)
 (for*/vector 1)
 (for/and 1)
 (for/first 1)
 (for/fold 2)
 (for/hash 1)
 (for/hasheq 1)
 (for/hasheqv 1)
 (for/last 1)
 (for/list 1)
 (for/lists 2)
 (for/or 1)
 (for/product 1)
 (for/sum 1)
 (for/vector 1)
 (instantiate 2)
 (interface 1)
 (lambda/kw 1)
 (lambda: 1)
 (let*-values: 1)
 (let+ 1)
 (let-values: 1)
 (let/cc: 1)
 (let: 1)
 (letrec-values: 1)
 (letrec: 1)
 (local 1)
 (mixin 2)
 (module defun)
 (module+ defun)
 (parameterize-break 1)
 (quasisyntax/loc 1)
 (send* 1)
 (splicing-let 1)
 (splicing-let-syntax 1)
 (splicing-let-syntaxes 1)
 (splicing-let-values 1)
 (splicing-letrec 1)
 (splicing-letrec-syntax 1)
 (splicing-letrec-syntaxes 1)
 (splicing-letrec-syntaxes+values 1)
 (splicing-letrec-values 1)
 (splicing-local 1)
 (syntax-id-rules defun)
 (syntax/loc 1)
 (type-case defun)
 (unit defun)
 (unit/sig 2)
 (with-handlers 1)
 (with-handlers: 1))



;;; Implementation definition:

(define-geiser-implementation racket
  (unsupported-procedures '(callers callees generic-methods))
  (binary geiser-racket--binary)
  (arglist geiser-racket--parameters)
  (repl-startup geiser-racket--startup)
  (prompt-regexp geiser-racket--prompt-regexp)
  (marshall-procedure geiser-racket--geiser-procedure)
  (find-module geiser-racket--get-module)
  (enter-command geiser-racket--enter-command)
  (import-command geiser-racket--import-command)
  (exit-command geiser-racket--exit-command)
  (find-symbol-begin geiser-racket--symbol-begin)
  (display-error geiser-racket--display-error)
  (external-help geiser-racket--external-help)
  (check-buffer geiser-racket--guess)
  (keywords geiser-racket--keywords)
  (binding-forms geiser-racket--binding-forms)
  (binding-forms* geiser-racket--binding-forms*))

(geiser-impl--add-to-alist 'regexp "\\.ss$" 'racket t)
(geiser-impl--add-to-alist 'regexp "\\.rkt$" 'racket t)

(defun run-gracket ()
  "Start the Racket REPL using gracket instead of plain racket."
  (interactive)
  (let ((geiser-racket-use-gracket-p t))
    (run-racket)))


(provide 'geiser-racket)
