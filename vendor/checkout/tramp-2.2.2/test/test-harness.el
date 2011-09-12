;; test-harness.el --- Run Emacs Lisp test suites.

;;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Martin Buchholz
;; Keywords: testing

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;;; A test suite harness for testing XEmacs.
;;; The actual tests are in other files in this directory.
;;; Basically you just create files of emacs-lisp, and use the
;;; Assert, Check-Error, and Check-Message functions to create tests.
;;; You run the tests using M-x test-emacs-test-file,
;;; or $(EMACS) -batch -l .../test-harness.el -f batch-test-emacs file ...
;;; which is run for you by the `make check' target in the top-level Makefile.

(require 'bytecomp)

(defvar test-harness-verbose
  (and (not noninteractive) (> (device-baud-rate) search-slow-speed))
  "*Non-nil means print messages describing progress of emacs-tester.")

(defvar test-harness-current-file nil)

(defvar emacs-lisp-file-regexp (purecopy "\\.el\\'")
  "*Regexp which matches Emacs Lisp source files.")

;;;###autoload
(defun test-emacs-test-file (filename)
  "Test a file of Lisp code named FILENAME.
The output file's name is made by appending `c' to the end of FILENAME."
  (interactive
   (let ((file buffer-file-name)
	 (file-name nil)
	 (file-dir nil))
     (and file
	  (eq (cdr (assq 'major-mode (buffer-local-variables)))
	      'emacs-lisp-mode)
	  (setq file-name (file-name-nondirectory file)
		file-dir (file-name-directory file)))
     (list (read-file-name "Test file: " file-dir nil nil file-name))))
  ;; Expand now so we get the current buffer's defaults
  (setq filename (expand-file-name filename))

  ;; If we're testing a file that's in a buffer and is modified, offer
  ;; to save it first.
  (or noninteractive
      (let ((b (get-file-buffer (expand-file-name filename))))
	(if (and b (buffer-modified-p b)
		 (y-or-n-p (format "save buffer %s first? " (buffer-name b))))
	    (save-excursion (set-buffer b) (save-buffer)))))

  (if (or noninteractive test-harness-verbose)
      (message "Testing %s..." filename))
  (let ((test-harness-current-file filename)
	input-buffer)
    (save-excursion
      (setq input-buffer (get-buffer-create " *Test Input*"))
      (set-buffer input-buffer)
      (erase-buffer)
      (insert-file-contents filename)
      ;; Run hooks including the uncompression hook.
      ;; If they change the file name, then change it for the output also.
      (let ((buffer-file-name filename)
	    (default-major-mode 'emacs-lisp-mode)
	    (enable-local-eval nil))
        (normal-mode)
        (setq filename buffer-file-name)))
    (test-harness-from-buffer input-buffer filename)
    (kill-buffer input-buffer)
    ))

(defun test-harness-read-from-buffer (buffer)
  "Read forms from BUFFER, and turn it into a lambda test form."
  (let ((body nil))
    (goto-char (point-min) buffer)
    (condition-case error-info
	(while t
	  (setq body (cons (read buffer) body)))
      (end-of-file nil)
      (error
       (princ "Unexpected error %S reading forms from buffer\n" error-info)))
    `(lambda ()
       (defvar passes)
       (defvar assertion-failures)
       (defvar no-error-failures)
       (defvar wrong-error-failures)
       (defvar missing-message-failures)
       (defvar other-failures)

       (defvar unexpected-test-suite-failure)
       (defvar trick-optimizer)

       ,@(nreverse body))))

(defun test-harness-from-buffer (inbuffer filename)
  "Run tests in buffer INBUFFER, visiting FILENAME."
  (defvar trick-optimizer)
  (let ((passes 0)
	(assertion-failures 0)
	(no-error-failures 0)
	(wrong-error-failures 0)
	(missing-message-failures 0)
	(other-failures 0)

	(trick-optimizer nil)
	(unexpected-test-suite-failure nil)
	(debug-on-error t))
    (with-output-to-temp-buffer "*Test-Log*"

      (defmacro Assert (assertion)
	`(condition-case error-info
	     (progn
	       (assert ,assertion)
	       (princ (format "PASS: %S" (quote ,assertion)))
	       (terpri)
	       (incf passes))
	   (cl-assertion-failed
	    (princ (format "FAIL: Assertion failed: %S\n" (quote ,assertion)))
	    (incf assertion-failures))
	   (t (princ (format "FAIL: %S ==> error: %S\n" (quote ,assertion) error-info))
	      (incf other-failures)
	      )))

      (defmacro Check-Error (expected-error &rest body)
	(let ((quoted-body (if (= 1 (length body))
			       `(quote ,(car body)) `(quote (progn ,@body)))))
	  `(condition-case error-info
	       (progn
		 (setq trick-optimizer (progn ,@body))
		 (princ (format "FAIL: %S executed successfully, but expected error %S\n"
				,quoted-body
				',expected-error))
		 (incf no-error-failures))
	     (,expected-error
	      (princ (format "PASS: %S ==> error %S, as expected\n"
			     ,quoted-body ',expected-error))
	      (incf passes))
	     (error
	      (princ (format "FAIL: %S ==> expected error %S, got error %S instead\n"
			     ,quoted-body ',expected-error error-info))
	      (incf wrong-error-failures)))))

      (defmacro Check-Error-Message (expected-error expected-error-regexp &rest body)
	(let ((quoted-body (if (= 1 (length body))
			       `(quote ,(car body)) `(quote (progn ,@body)))))
	  `(condition-case error-info
	       (progn
		 (setq trick-optimizer (progn ,@body))
		 (princ (format "FAIL: %S executed successfully, but expected error %S\n"
				,quoted-body
				',expected-error))
		 (incf no-error-failures))
	     (,expected-error
	      (let ((error-message (second error-info)))
		(if (string-match ,expected-error-regexp error-message)
		    (progn
		      (princ (format "PASS: %S ==> error %S %S, as expected\n"
				     ,quoted-body error-message ',expected-error))
		      (incf passes))
		  (princ (format "FAIL: %S ==> got error %S as expected, but error message %S did not match regexp %S\n"
				 ,quoted-body ',expected-error error-message ,expected-error-regexp))
		  (incf wrong-error-failures))))
	     (error
	      (princ (format "FAIL: %S ==> expected error %S, got error %S instead\n"
			     ,quoted-body ',expected-error error-info))
	      (incf wrong-error-failures)))))


      (defmacro Check-Message (expected-message-regexp &rest body)
	(let ((quoted-body (if (= 1 (length body))
			       `(quote ,(car body)) `(quote (progn ,@body)))))
	  `(let ((messages ""))
	     (defadvice message (around collect activate)
	       (defvar messages)
	       (let ((msg-string (apply 'format (ad-get-args 0))))
		 (setq messages (concat messages msg-string))
		 msg-string))
	     (condition-case error-info
		 (progn
		   (setq trick-optimizer (progn ,@body))
		   (if (string-match ,expected-message-regexp messages)
		       (progn
			 (princ (format "PASS: %S ==> value %S, message %S, matching %S, as expected\n"
					,quoted-body trick-optimizer messages ',expected-message-regexp))
			 (incf passes))
		     (princ (format "FAIL: %S ==> value %S, message %S, NOT matching expected %S\n"
				    ,quoted-body  trick-optimizer messages ',expected-message-regexp))
		     (incf missing-message-failures)))
	       (error
		(princ (format "FAIL: %S ==> unexpected error %S\n"
			       ,quoted-body error-info))
		(incf other-failures)))
	     (ad-unadvise 'message))))

      (defmacro Ignore-Ebola (&rest body)
	`(let ((debug-issue-ebola-notices -42)) ,@body))

      (defun Int-to-Marker (pos)
	(save-excursion
	  (set-buffer standard-output)
	  (save-excursion
	    (goto-char pos)
	    (point-marker))))

      (princ "Testing Interpreted Lisp\n\n")
      (condition-case error-info
	  (funcall (test-harness-read-from-buffer inbuffer))
	(error
	 (setq unexpected-test-suite-failure t)
	 (princ (format "Unexpected error %S while executing interpreted code\n"
		error-info))
	 (message "Unexpected error %S while executing interpreted code." error-info)
	 (message "Test suite execution aborted." error-info)
	 ))
      (princ "\nTesting Compiled Lisp\n\n")
      (let (code)
	(condition-case error-info
	    (setq code
		  ;; our lisp code is often intentionally dubious,
		  ;; so throw away _all_ the byte compiler warnings.
		  (letf (((symbol-function 'byte-compile-warn) 'ignore))
		    (byte-compile (test-harness-read-from-buffer inbuffer))))
	  (error
	   (princ (format "Unexpected error %S while byte-compiling code\n"
			  error-info))))
	(condition-case error-info
	    (if code (funcall code))
	  (error
	   (princ (format "Unexpected error %S while executing byte-compiled code\n"
			  error-info))
	   (message "Unexpected error %S while executing byte-compiled code." error-info)
	   (message "Test suite execution aborted." error-info)
	   )))
      (princ "\nSUMMARY:\n")
      (princ (format "\t%5d passes\n" passes))
      (princ (format "\t%5d assertion failures\n" assertion-failures))
      (princ (format "\t%5d errors that should have been generated, but weren't\n" no-error-failures))
      (princ (format "\t%5d wrong-error failures\n" wrong-error-failures))
      (princ (format "\t%5d missing-message failures\n" missing-message-failures))
      (princ (format "\t%5d other failures\n" other-failures))
      (let* ((total (+ passes
		       assertion-failures
		       no-error-failures
		       wrong-error-failures
		       missing-message-failures
		       other-failures))
	     (basename (file-name-nondirectory filename))
	     (summary-msg
	      (if (> total 0)
		  (format "%s: %d of %d (%d%%) tests successful."
			  basename passes total (/ (* 100 passes) total))
		(format "%s: No tests run" basename))))
	(message "%s" summary-msg))
      (when unexpected-test-suite-failure
	(message "Test suite execution failed unexpectedly."))
      (fmakunbound 'Assert)
      (fmakunbound 'Check-Error)
      (fmakunbound 'Ignore-Ebola)
      (fmakunbound 'Int-to-Marker)
      )))

(defvar test-harness-results-point-max nil)
(defmacro displaying-emacs-test-results (&rest body)
  `(let ((test-harness-results-point-max test-harness-results-point-max))
     ;; Log the file name.
     (test-harness-log-file)
     ;; Record how much is logged now.
     ;; We will display the log buffer if anything more is logged
     ;; before the end of BODY.
     (or test-harness-results-point-max
	 (save-excursion
	   (set-buffer (get-buffer-create "*Test-Log*"))
	   (setq test-harness-results-point-max (point-max))))
     (unwind-protect
	 (condition-case error-info
	     (progn ,@body)
	   (error
	    (test-harness-report-error error-info)))
       (save-excursion
	 ;; If there were compilation warnings, display them.
	 (set-buffer "*Test-Log*")
	 (if (= test-harness-results-point-max (point-max))
	     nil
	   (if temp-buffer-show-function
	       (let ((show-buffer (get-buffer-create "*Test-Log-Show*")))
		 (save-excursion
		   (set-buffer show-buffer)
		   (setq buffer-read-only nil)
		   (erase-buffer))
		 (copy-to-buffer show-buffer
				 (save-excursion
				   (goto-char test-harness-results-point-max)
				   (forward-line -1)
				   (point))
				 (point-max))
		 (funcall temp-buffer-show-function show-buffer))
              (select-window
               (prog1 (selected-window)
                 (select-window (display-buffer (current-buffer)))
                 (goto-char test-harness-results-point-max)
                 (recenter 1)))))))))

(defun batch-test-emacs-1 (file)
  (condition-case error-info
      (progn (test-emacs-test-file file) t)
    (error
     (princ ">>Error occurred processing ")
     (princ file)
     (princ ": ")
     (display-error error-info nil)
     (terpri)
     nil)))

(defun batch-test-emacs ()
  "Run `test-harness' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.
Each file is processed even if an error occurred previously.
For example, invoke \"xemacs -batch -f batch-test-emacs tests/*.el\""
  ;; command-line-args-left is what is left of the command line (from
  ;; startup.el)
  (defvar command-line-args-left)	;Avoid 'free variable' warning
  (defvar debug-issue-ebola-notices)
  (if (not noninteractive)
      (error "`batch-test-emacs' is to be used only with -batch"))
  (let ((error nil))
    (dolist (file command-line-args-left)
      (if (file-directory-p file)
	  (dolist (file-in-dir (directory-files file t))
	    (when (and (string-match emacs-lisp-file-regexp file-in-dir)
		       (not (or (auto-save-file-name-p file-in-dir)
				(backup-file-name-p file-in-dir)
				(equal (file-name-nondirectory file-in-dir)
				       "test-harness.el"))))
	      (or (batch-test-emacs-1 file-in-dir)
		  (setq error t))))
	(or (batch-test-emacs-1 file)
	    (setq error t))))
    ;;(message "%s" (buffer-string nil nil "*Test-Log*"))
    (message "Done")
    (kill-emacs (if error 1 0))))

(provide 'test-harness)

;;; test-harness.el ends here
