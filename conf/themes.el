;;; themes.el

(set-fontset-font "fontset-default" 'unicode bg--nerd-font nil 'prepend)
(set-fontset-font "fontset-default" 'unicode bg--emoji-font nil 'prepend)

(defun bg/disable-themes ()
  "Disable all enabled custom themes."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))


(use-package fontaine
  :demand t
  :if window-system
  :init
  (setq fontaine-presets
        `((regular
           :default-height ,bg--default-font-size)
          (monitor
           :default-height 250
           :bold-weight bold)
          (t
           :default-family ,bg--default-font
           :default-weight normal
           :default-width semi-condensed
           :variable-pitch-family ,bg--variable-pitch-font
           :italic-family ,bg--default-font
           :italic-slant oblique        ;specific to Berkeley Mono
           :variable-pitch-height 1.05)))
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))


(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-hud t)
  (doom-modeline-height 25)
  (doom-modeline-bar-width 6)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-window-width-limit 115)
  (doom-modeline-vcs-max-length 25)
  (doom-modeline-project-detection 'project)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (doom-modeline-env-version nil))


(use-package ef-themes
  :straight (ef-themes :type git :host github :repo "protesilaos/ef-themes" :branch "1.10.0")
  :demand t
  :custom
  (ef-themes-region '(intense no-extend neutral))
  (ef-themes-variable-pitch-ui nil)
  (ef-themes-disable-other-themes t)
  (ef-themes-to-toggle '(ef-dream ef-light))
  :init
  ;; NOTE: Keeping this only for documentation purposes, actual setup happens with `hl-todo`
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
  (bg/disable-themes)
  :config
  (ef-themes-select 'ef-dream)
  ;; OKAY: Set the cursor to theme red
  (set-cursor-color (ef-themes-get-color-value 'red))

  ;; Custom wrapping colors
  (defface bg/custom-curly-face
    `((t (:foreground ,(ef-themes-get-color-value 'fg-dim))))
    "Face for fringe curly bitmaps."
    :group 'basic-faces)
  (set-fringe-bitmap-face 'right-curly-arrow 'bg/custom-curly-face)
  (set-fringe-bitmap-face 'left-curly-arrow 'bg/custom-curly-face))


(use-package highlight-indent-guides
  :straight t
  :commands (highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-auto-enabled nil)
  (highlight-indent-guides-character #x258f)
  :config
  (set-face-foreground 'highlight-indent-guides-character-face (ef-themes-get-color-value 'bg-active))
  (set-face-foreground 'highlight-indent-guides-top-character-face (ef-themes-get-color-value 'fg-dim)))


(use-package indent-bars
  :straight t
  :commands (indent-bars-mode)
  :custom
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-prefer-character t)
  (indent-bars-color '(highlight :face-bg t :blend 0.25))
  (indent-bars-highlight-current-depth '(:face default :blend 0.4))
  (indent-bars-color-by-depth nil))


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
                            "<=" ">=" "<=>" "=>"
                            "??"
                            "->" "<-" "<--" "-->" "<>" "->>" "-<" "(->" "(->>"
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


(use-package spacious-padding
  :straight (spacious-padding :type git :host github :repo "protesilaos/spacious-padding")
  :demand t
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '( :internal-border-width 24
      :header-line-width 4
      :mode-line-width 10
      :tab-width 4
      :right-divider-width 30
      :scroll-bar-width 12
      :fringe-width 12)))


;; Replacement for `golden-ratio`
(use-package zoom
  :straight t
  :hook (after-init . zoom-mode)
  :init
  (defun size-callback ()
    (cond ((> (frame-pixel-width) 1280) '(100 . 0.75))
          (t                            '(0.618 . 0.618))))
  :custom
  (zoom-ignored-major-modes '(dired-mode markdown-mode magit-mode))
  (zoom-ignored-buffer-names '("*wclock*"))
  (zoom-size #'size-callback))


(use-package hide-mode-line
  :straight t
  :hook
  (treemacs-mode . hide-mode-line-mode))
