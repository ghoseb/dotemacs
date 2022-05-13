;;; themes.el

(set-face-attribute 'default nil
                    :family bg/default-font
                    :height bg/default-font-size
                    :weight 'regular)

(set-face-attribute 'fixed-pitch nil
                    :font bg/fixed-pitch-font)

(set-face-attribute 'variable-pitch nil
                    :font bg/variable-pitch-font)

(set-fontset-font "fontset-default" 'unicode bg/emoji-font nil 'prepend)


(defun bg/switch-theme-to-default ()
  "Switch to default dark theme."
  (interactive)
  (load-theme bg/default-theme t))


(defun bg/switch-theme-to-alternative ()
  "Switch to alternate light theme."
  (interactive)
  (load-theme bg/alternative-theme t))


(use-package all-the-icons
  :straight t
  :defer 10
  :if (display-graphic-p))


(use-package all-the-icons-completion
  :straight t
  :defer t
  :after all-the-icons
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))


(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook
  (dired-mode . all-the-icons-dired-mode))


(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height 10)))


(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  :init
  (load-theme bg/default-theme t))
