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


(use-package all-the-icons
  :straight t
  :defer 10
  :if (display-graphic-p)
  :custom
  (all-the-icons-scale-factor 1.1))


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
  (doom-modeline-height 25)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-window-width-limit 115)
  (doom-modeline-project-detection 'project)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  :custom-face
  (mode-line ((t (:height 0.95))))
  (mode-line-active ((t (:height 0.95))))
  (mode-line-inactive ((t (:height 0.95)))))


(use-package ef-themes
  :straight t
  :demand t
  :custom
  (ef-themes-region '(intense no-extend neutral))
  (ef-themes-variable-pitch-ui nil)
  (ef-themes-disable-other-themes t)
  :init
  (defun bg/ef-themes-hl-todo-faces ()
    "Configure `hl-todo-keyword-faces' with Ef themes colors."
    (ef-themes-with-colors
      (setq hl-todo-keyword-faces
            `(("HOLD" . ,yellow)
              ("TODO" . ,red)
              ("NEXT" . ,blue)
              ("OKAY" . ,green-warmer)
              ("DONT" . ,yellow-warmer)
              ("FAIL" . ,red-warmer)
              ("BUG" . ,red-warmer)
              ("DONE" . ,green)
              ("NOTE" . ,blue-warmer)
              ("HACK" . ,cyan)
              ("FIXME" . ,red-warmer)
              ("XXX" . ,red-warmer)
              ("DEPRECATED" . ,yellow)))))
  :config
  (add-hook 'ef-themes-post-load-hook #'bg/ef-themes-hl-todo-faces)
  (ef-themes-select 'ef-elea-dark))


(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-auto-enabled nil)
  (highlight-indent-guides-character #x258f)
  :config
  (set-face-foreground 'highlight-indent-guides-character-face (ef-themes-get-color-value 'bg-active))
  (set-face-foreground 'highlight-indent-guides-top-character-face (ef-themes-get-color-value 'fg-dim)))


(defun bg/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (bg/disable-themes)
  (pcase appearance
    ('light (ef-themes-select 'ef-elea-light))
    ('dark (ef-themes-select 'ef-elea-dark))))

;; (add-hook 'ns-system-appearance-change-functions #'bg/apply-theme)

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
                            "#{" "#(" "#_" "#_(" "#?" "#:" "~@" ";;" ";;;"
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
