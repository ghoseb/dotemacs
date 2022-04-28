;;; themes.el

(set-face-attribute 'default nil
                    :family bg/default-font
                    :height bg/default-font-size)

(set-face-attribute 'fixed-pitch nil
                    :font bg/fixed-pitch-font)

(set-face-attribute 'variable-pitch nil
                    :font bg/variable-pitch-font)

(use-package all-the-icons
  :straight t
  :defer 10
  :if (display-graphic-p))


(use-package all-the-icons-completion
  :straight t
  :defer t
  :after all-the-icons
  :init
  (all-the-icons-completion-mode))


(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height 10)))


(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  :init
  (load-theme bg/default-theme t))
