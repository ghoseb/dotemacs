(require 'sml-mode)

(setenv "PATH" (concat "/usr/local/Cellar/smlnj/110.75/libexec/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/local/Cellar/smlnj/110.75/libexec/bin"  exec-path))

;; README
;; To load a file and run sml repl -> Press C-c C-v inside a sml file.
;; To run the current files tests -> Press C-c C-r inside a sml file. It assumes the tests are inside the same directory.
;; For example, If your code file is hw1.sml your test file should be named as hw1tests.sml in the same directory

;;; Credits: https://gist.github.com/koddo/4555655
(defun my-sml-restart-repl-and-load-current-file ()
  (interactive)
  (save-buffer)
  (ignore-errors (with-current-buffer "*sml*"
                   (comint-interrupt-subjob)
                   (comint-send-eof)
                   (let ((some-time 0.1))
                     (while (process-status (get-process "sml"))
                       (sleep-for some-time)))))
  (flet ((sml--read-run-cmd ()
           '("sml" "" nil))) ; (command args host)
    (sml-prog-proc-send-buffer t)))

(defun my-sml-restart-repl-and-load-current-file-tests ()
  (interactive)
  (save-buffer)
  (let ((code-file-name (replace-regexp-in-string "tests.sml$" ".sml" (buffer-file-name))))
    (let ((test-file-name (replace-regexp-in-string ".sml$" "tests.sml" code-file-name))
          (code-buffer (buffer-name)))
      (find-file test-file-name)
      (my-sml-restart-repl-and-load-current-file)
      (switch-to-buffer-other-window code-buffer))))

(eval-after-load 'sml-mode
  '(progn
    (define-key sml-mode-map (kbd "C-j") 'reindent-then-newline-and-indent)
    (define-key sml-mode-map (kbd "C-c C-s") 'sml-run)
    (define-key sml-mode-map (kbd "C-c C-v") 'my-sml-restart-repl-and-load-current-file)
    (define-key sml-mode-map (kbd "C-c C-r") 'my-sml-restart-repl-and-load-current-file-tests)))
