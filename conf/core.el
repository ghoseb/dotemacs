;;; core.el

(use-package emacs
  :hook
  ('before-save . #'delete-trailing-whitespace)
  :config
  (setq-default
   indent-tabs-mode nil
   fill-column 115
   truncate-string-ellipsis "…"
   sentence-end-double-space nil
   cursor-type '(hbar .  2)
   cursor-in-non-selected-windows nil)
  (setq
   tab-width 4
   tab-always-indent 'complete
   require-final-newline t
   custom-safe-themes t
   confirm-kill-emacs #'yes-or-no-p
   dired-kill-when-opening-new-dired-buffer t
   completion-cycle-threshold 3
   tab-always-indent 'complete
   version-control t
   kept-new-versions 10
   kept-old-versions 0
   delete-old-versions t
   vc-make-backup-files t
   make-backup-files nil
   use-dialog-box nil
   global-auto-revert-non-file-buffers t
   blink-cursor-mode nil
   history-delete-duplicates t
   default-directory "~/"
   confirm-kill-processes nil)
  (delete-selection-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  ;; (global-hl-line-mode 1)
  (global-auto-revert-mode 1)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment   'utf-8)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (defun prot/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

    The generic `keyboard-quit' does not do the expected thing when
    the minibuffer is open.  Whereas we want it to close the
    minibuffer, even without explicitly focusing it.

    The DWIM behaviour of this command is as follows:

      - When the region is active, disable it.
      - When a minibuffer is open, but not focused, close the minibuffer.
      - When the Completions buffer is selected, close it.
      - In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))
  :bind
  ("C-g" . #'prot/keyboard-quit-dwim)
  ("C-c q" . #'bury-buffer)
  ("<escape>" . #'keyboard-escape-quit))


(use-package exec-path-from-shell
  :straight t
  :defer 2
  :config
  (exec-path-from-shell-initialize))


;; Keep .emacs.d clean
(use-package no-littering
  :straight t
  :demand t
  :init
  (setq no-littering-etc-directory (expand-file-name "config/"  bg--save-dir)
        no-littering-var-directory (expand-file-name "data/" bg--save-dir))
  :config
  (eval-after-load "recentf"
    '(progn
       (add-to-list 'recentf-exclude no-littering-var-directory)
       (add-to-list 'recentf-exclude no-littering-etc-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))


(use-package prescient
  :straight t
  :demand t
  :custom
  (prescient-filter-method '(literal initialism prefix regexp fuzzy))
  (prescient-history-length 1000)
  (prescient-use-char-folding t)
  (prescient-use-case-folding 'smart)
  (prescient-sort-full-matches-first t)
  (prescient-sort-length-enable t)
  (prescient-save-file
   (expand-file-name "prescient-save.el"
                     no-littering-var-directory))
  :config
  (prescient-persist-mode +1))


(use-package vertico
  :straight '(vertico :files (:defaults "extensions/*")
                      :includes (vertico-buffer
                                 vertico-directory
                                 vertico-flat
                                 vertico-grid
                                 vertico-indexed
                                 vertico-mouse
                                 vertico-quick
                                 vertico-repeat
                                 vertico-reverse))
  :bind (:map vertico-map
              ("M-." . vertico-repeat)
              ("C-n" . vertico-next)
              ("C-p" . vertico-previous)
              ("C-f" . vertico-exit)
              ("C-M-n" . vertico-next-group)
              ("C-M-p" . vertico-previous-group)
              ("C-<backspace>" . vertico-directory-delete-word))
  :hook
  (minibuffer-setup . vertico-repeat-save)
  :custom
  (vertico-cycle t)
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-preselect 'first)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :init
  (vertico-mode)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))


(use-package vertico-prescient
  :straight t
  :after vertico
  :config
  (vertico-prescient-mode 1))


(use-package marginalia
  :after vertico
  :straight t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy
                                marginalia-annotators-light
                                nil)))


(use-package ctrlf
  :straight t
  :bind (("C-s" . ctrlf-forward-default)
         ("C-M-s" . ctrlf-forward-alternate)
         ("C-r" . ctrlf-backward-default)
         ("C-M-r" . ctrlf-backward-alternate))
  :config (ctrlf-mode +1))


(use-package blackout
  :straight t
  :demand t
  :config
  (blackout 'auto-fill-mode)
  (blackout 'eldoc-mode)
  (blackout 'emacs-lisp-mode "EL"))


(use-package nerd-icons-corfu
  :straight t)

(use-package corfu
  :straight (corfu :repo "minad/corfu" :branch "main" :files (:defaults "extensions/*.el"))
  :config
  (defun corfu-complete-and-quit ()
    (interactive)
    (corfu-complete)
    (corfu-quit))
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode +1)
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("C-p" . corfu-previous)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET" . corfu-complete-and-quit)
              ("<return>" . corfu-complete-and-quit)
              ("C-g" . corfu-quit)
              ("C-q" . corfu-quick-insert)
              ("S-SPC" . corfu-insert-separator)
              ([remap completion-at-point] . corfu-complete)
              ("M-d" . corfu-popupinfo-toggle)
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up))
  :custom
  (corfu-cycle nil)
  (corfu-auto t)
  (corfu-count 9)
  (corfu-on-exact-match 'quit)
  (corfu-preselect-first t)
  (corfu-quit-at-boundary 'separator)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5))


(use-package corfu-prescient
  :straight t
  :after (prescient corfu)
  :demand t
  :init
  (corfu-prescient-mode +1))


(use-package cape
  :demand t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))


(use-package kind-icon
  :straight t
  :demand t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.75 :scale 0.85 :background nil))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package orderless
  :straight t
  :after (vertico prescient)
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion basic))))
  (orderless-component-separator 'orderless-escapable-split-on-space))


(use-package which-key
  :straight t
  :hook (emacs-startup . which-key-mode)
  :custom
  (which-key-popup-type 'side-window))


(use-package treemacs
  :straight t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     1
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name "treemacs-persist" no-littering-var-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               60
          treemacs-width                           30
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 20)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-commit-diff-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
    (treemacs-hide-gitignored-files-mode t))
  (treemacs-project-follow-mode 1)
  :bind
  (:map global-map
        ("s-t"       . treemacs-add-and-display-current-project)
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package treemacs-icons-dired
  :straight t
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)


(use-package treemacs-magit
  :straight t
  :hook treemacs
  :after (treemacs magit)
  :ensure t)


(use-package treemacs-all-the-icons
  :straight t
  :after treemacs)


(use-package whitespace
  :disabled t
  :straight t
  :commands (whitespace-mode)
  :hook ((prog-mode . whitespace-mode)
         (text-mode . whitespace-mode)
         (before-save . whitespace-cleanup))
  :config
  (setq whitespace-line-column 115)
  (setq whitespace-style '(face tabs empty trailing lines-tail)))


(use-package multiple-cursors
  :straight t
  :hook (prog-mode . multiple-cursors-mode)
  :bind
  (("C-M-s-. C-M-s-." . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)     ;FIXME: conflicts with paredit
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))


(use-package super-save
  :straight t
  :init
  (super-save-mode 1)
  :config
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil))


(use-package saveplace
  :straight (:type built-in)
  :init
  (save-place-mode 1)
  :config
  (setq-default save-place t))


(use-package savehist
  :straight (:type built-in)
  :demand t
  :init
  (savehist-mode 1)
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring kill-ring mark-ring)
        savehist-autosave-interval 60))


(use-package recentf
  :straight (:type built-in)
  :demand t
  :init
  (recentf-mode t)
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 50
        recentf-auto-cleanup 60)
  (add-to-list 'recentf-exclude bg--local-dir)
  (add-to-list 'recentf-exclude "/opt/homebrew/Cellar/"))


(use-package uniquify
  :straight (:type built-in)
  :custom (uniquify-buffer-name-style 'forward))


(use-package helpful
  :straight t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))


(use-package undo-fu
  :straight t
  :bind (("M-z" . undo-fu-only-undo)
         ("M-Z" . undo-fu-only-redo))
  :init
  (global-unset-key (kbd "C-z")))


(use-package vundo
  :straight t
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))


(use-package ace-window
  :straight t
  :bind
  ("M-o" . ace-window))


(use-package avy
  :straight t
  :bind
  ("M-g M-c" . avy-goto-char-timer)
  ("M-g M-g" . avy-goto-line)
  :config
  (setq avy-background t)
  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful))


(use-package outline-indent
  :straight t
  :commands (outline-indent-minor-mode)
  :hook ((prog-mode . outline-indent-minor-mode))
  :bind-keymap ("C-c f" . bg--outline-indent-keymap)
  :init
  (defvar-keymap bg--outline-indent-keymap
    :doc "Prefix map for Outline Indent Mode"
    "c" `("Close fold" . ,#'outline-indent-close-fold)
    "o" `("Open fold". ,#'outline-indent-open-fold)
    "O" `("Open fold recursive" . ,#'outline-indent-open-fold-rec)
    "M-c" `("Close all folds" . ,#'outline-indent-close-folds)
    "M-o" `("Open all folds" . ,#'outline-indent-open-folds))
  :custom
  (outline-blank-line t)
  (outline-indent-ellipsis " ↘ "))


(use-package pulsar
  :straight t
  :defer 5
  :init
  (pulsar-global-mode 1)
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.05)
  (setq pulsar-iterations 13)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-highlight-face 'pulsar-green)
  :bind
  ("C-x l" . #'pulsar-pulse-line-red)
  ("C-c h h" . #'pulsar-highlight-dwim)
  :hook
  ((next-error . #'pulsar-pulse-line)
   (minibuffer-setup . #'pulsar-pulse-line)))


(use-package expand-region
  :straight t
  :bind
  ("C-=" . #'er/expand-region))


(use-package consult
  :bind ;; C-c bindings in `mode-specific-map'
  ("C-c M-x" . consult-mode-command)
  ("C-x M-f" . consult-recent-file)
  ("C-c k" . consult-kmacro)
  ("C-c m" . consult-man)
  ("C-c i" . consult-info)
  ([remap Info-search] . consult-info)
  ;; C-x bindings in `ctl-x-map'
  ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
  ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
  ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
  ;; Custom M-# bindings for fast register access
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
  ("C-M-#" . consult-register)
  ;; Other custom bindings
  ("M-y" . consult-yank-pop)    ;; orig. yank-pop
  ;; M-g bindings in `goto-map'
  ("M-g g" . consult-goto-line) ;; orig. goto-line

  ;; Minibuffer history
  (:map minibuffer-local-map
        ;; orig. next-matching-history-element
        ("C-r" . consult-history)
        ;; orig. previous-matching-history-element
        ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :custom
  (consult-narrow-key "C-,")
  (consult-widen-key "C-.")
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)

   consult-line
   consult-ripgrep
   :initial (when (use-region-p)
              (buffer-substring-no-properties
               (region-beginning) (region-end)))

   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))


(use-package scrollkeeper
  :straight t
  :bind
  ([remap scroll-up-command] . scrollkeeper-contents-up)
  ([remap scroll-down-command] . scrollkeeper-contents-down))


(use-package dired
  :straight (:type built-in)
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   ;; (dired-mode . hl-line-mode)
   )
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))


(use-package dired-subtree
  :straight t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))


(use-package trashed
  :straight t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))


(use-package disproject
  :straight t
  :after (project)
  :bind (:map ctl-x-map
              ("p" . disproject-dispatch)))


(use-package ultra-scroll
  :straight (ultra-scroll
             :type git
             :host github
             :repo "jdtsmith/ultra-scroll"
             :branch "main")
  :init
  (setq scroll-conservatively 101       ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))


(use-package compile-angel
  :straight t
  :ensure t
  :config
  (setq compile-angel-verbose t)
  (setq compile-angel-enable-byte-compile nil)
  (setq compile-angel-enable-native-compile t)
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))



(use-package lin
  :straight t
  :after (ef-themes)
  :demand t
  :commands (lin-mode)
  :custom
  (lin-mode-hooks '(prog-mode-hook))
  (lin-face 'lin-blue)
  :config
  (lin-global-mode +1))
