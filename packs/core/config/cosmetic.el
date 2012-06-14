;; set cursor colour
(set-cursor-color "yellow")

;; make sure ansi colour character escapes are honoured
(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;; get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;make evil tabs visible as arrows
(let ((d (make-display-table)))
  (aset d 9 (vector ?→ ? ))
  (set-window-display-table nil d))

;; Line-wrapping
(set-default 'fill-column 72)

(custom-set-variables
 '(term-default-bg-color "#000000")  ;; background color (black)
 '(term-default-fg-color "#dddd00")) ;; foreground color (yellow)


;; Cursor-type
;; Use a bar cursor when mark is active and a region exists.
(defun th-activate-mark-init ()
  (setq cursor-type 'bar))

(add-hook 'activate-mark-hook 'th-activate-mark-init)

(defun th-deactivate-mark-init ()
  (setq cursor-type 'box))

(add-hook 'deactivate-mark-hook 'th-deactivate-mark-init)
