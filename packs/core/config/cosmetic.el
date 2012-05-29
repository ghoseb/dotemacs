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
