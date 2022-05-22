;;; osx.el

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; if `gnu ls' is in path, use that for dired
(let ((bg--gnuls (executable-find "gls")))
  (if bg--gnuls
   (setq dired-use-ls-dired t
         insert-directory-program bg--gnuls
         dired-listing-switches "-aBhl --group-directories-first")
   (setq dired-use-ls-dired nil)))
