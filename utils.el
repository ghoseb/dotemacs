;;; utils.el --- Summary

;;; Commentary:

;;; A bunch of utility functions

;;; Code:

(defun bg/maybe-load (file)
  "Try loading elisp FILE if it exists."
  (if (file-exists-p (expand-file-name file bg--conf-dir))
      (load-file (expand-file-name file bg--conf-dir))))


(defun bg/num-packages-loaded ()
  "Calculate the number of packages loaded."
  (- (length load-path) (length bg--init-load-path)))


(defun bg/startup-time-str ()
  "Return the startup time as a formatted string."
  (format
    "%.3f"
    (float-time
     (time-subtract after-init-time before-init-time))))


(defun bg/display-startup-msg ()
  "Display the startup message."
  (message
   "GNU/Emacs (v%s) ready with %d packages in %s secs (%d GCs)."
   emacs-version
   (bg/num-packages-loaded)
   (bg/startup-time-str)
   gcs-done))


(defun ar/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (let* ((buffer-read-only)
           (image-path "~/.emacs.d/images/emacs.png")
           (image (create-image image-path))
           (size (image-size image))
           (height (cdr size))
           (width (car size))
           (top-margin (floor (/ (- (window-height) height) 2)))
           (left-margin (floor (/ (- (window-width) width) 2)))
           (title (format "Welcome to GNU/Emacs v%s!" emacs-version)))
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n ))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      (insert (make-string (floor (/ (- (window-width) (string-width title)) 2)) ?\ ))
      (insert title))
    (setq cursor-type nil)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "q") 'kill-this-buffer)))


(provide 'utils)
;;; utils.el Ends here
