;;; themes.el

(set-face-attribute 'default nil
                    :family bg--default-font
                    :height bg--default-font-size
                    :weight 'regular)

(set-face-attribute 'fixed-pitch nil
                    :font bg--fixed-pitch-font)

(set-face-attribute 'variable-pitch nil
                    :font bg--variable-pitch-font)

(set-fontset-font "fontset-default" 'unicode bg--emoji-font nil 'prepend)

(defun bg/disable-themes ()
  "Disable all enabled custom themes."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))


(defun bg/switch-theme-to-default ()
  "Switch to default dark theme."
  (interactive)
  (bg/disable-themes)
  (load-theme bg--default-theme t))


(defun bg/switch-theme-to-alternative ()
  "Switch to alternate light theme."
  (interactive)
  (bg/disable-themes)
  (load-theme bg--alternative-theme t))


(use-package all-the-icons
  :straight t
  :defer 10
  :if (display-graphic-p)
  :custom
  (all-the-icons-scale-factor 1.0))


(use-package all-the-icons-completion
  :straight t
  :after (marginalia all-the-icons)
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
  :custom
  (doom-modeline-hud t)
  (doom-modeline-height 10)
  (doom-modeline-buffer-encoding t)
  :custom-face
  (mode-line ((t (:height 0.95))))
  (mode-line-active ((t (:height 0.95))))
  (mode-line-inactive ((t (:height 0.95)))))


(use-package doom-themes
  :straight t
  :disabled t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package kaolin-themes
  :straight t
  :demand t
  :custom
  (kaolin-themes-bold t)
  (kaolin-themes-italic t)
  (kaolin-themes-underline nil)
  (kaolin-themes-italic-comments t)
  (kaolin-themes-distinct-fringe t)
  (kaolin-themes-git-gutter-solid t)
  (kaolin-themes-treemacs-hl-line t)
  (kaolin-themes-comments-style 'contrast)
  (kaolin-themes-distinct-parentheses t)
  (kaolin-theme-linum-hl-line-style t)
  (kaolin-themes-hl-line-colored t)
  :config
  (kaolin-treemacs-theme))


(defun bg/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (bg/disable-themes)
  (pcase appearance
    ('light (load-theme bg--light-theme t))
    ('dark (load-theme bg--dark-theme t))))

;; (add-hook 'ns-system-appearance-change-functions #'bg/apply-theme)
(load-theme bg--default-theme t)

(use-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :demand t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'org-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures '(html-mode nxml-mode web-mode) '("<!--" "-->" "</>" "</" "/>" "://"))
  ;; reduced set of useful ligatures
  (ligature-set-ligatures 'prog-mode
                          '("++" "--" "/=" "&&" "||" "||="
                            "<<" "<<<" "<<=" ">>" ">>>" ">>=" "|=" "^="
                            "#{" "#(" "#_" "#_(" "#?" "#:" ";;" "~@"
                            "/*" "*/" "/**" "//" "///"
                            "<=" ">=" "<=>"
                            "??"
                            "<--" "-->" "<>"
                            "==" "===" "!=" "=/=" "!=="
                            "%%"
                            ":="
                            "**"
                            "!!"
                            "##" "###" "####" "---"
                            "#!"
                            ".." "..."
                            "__" "::"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))
