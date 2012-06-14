(setq custom-theme-directory (live-pack-lib-dir))

(load-theme 'tomorrow-night t)

;;; uncomment the following if you want zenburn
;; (load-theme 'zenburn t)
;; (set-cursor-color "firebrick")
;; (setq hcz-set-cursor-color-color "")
;; (setq hcz-set-cursor-color-buffer "")

;; (defun my-set-cursor-color ()
;;   "Change cursor color according to themes/init.el"
;;   ;; set-cursor-color is somewhat costly, so we only call it when needed:
;;   (let ((color "firebrick"))
;;     (unless (and
;;              (string= color hcz-set-cursor-color-color)
;;              (string= (buffer-name) hcz-set-cursor-color-buffer))
;;       (set-cursor-color (setq hcz-set-cursor-color-color color))
;;       (setq hcz-set-cursor-color-buffer (buffer-name)))))

;; (add-hook 'post-command-hook 'my-set-cursor-color)
