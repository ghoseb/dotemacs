;;; core.el

(use-package emacs
  :config
  (setq tab-width 4
        tab-always-indent 'complete
        require-final-newline t
        custom-safe-themes t
        confirm-kill-emacs #'yes-or-no-p
        dired-kill-when-opening-new-dired-buffer t
        completion-cycle-threshold 3
        tab-always-indent 'complete)
  (setq version-control t
        kept-new-versions 10
        kept-old-versions 0
        delete-old-versions t
        backup-by-copying t
        vc-make-backup-files t)
  (setq-default indent-tabs-mode nil
                fill-column 115
                truncate-string-ellipsis "…"
                sentence-end-double-space nil)
  (delete-selection-mode t)

  (column-number-mode t)
  (size-indication-mode t)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))


;; Keep .emacs.d clean
(use-package no-littering
  :straight t
  :demand t
  :init
  (setq no-littering-etc-directory (expand-file-name "config/"  bg/save-dir)
        no-littering-var-directory (expand-file-name "data/" bg/save-dir))
  :config
  (eval-after-load "recentf"
    '(progn
       (add-to-list 'recentf-exclude no-littering-var-directory)
       (add-to-list 'recentf-exclude no-littering-etc-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package selectrum
  :straight t
  :defer t
  :bind
  (("C-x C-z" . selectrum-repeat)
   :map selectrum-minibuffer-map
   ("C-r" . selectrum-select-from-history)
   :map minibuffer-local-map
   ("M-h" . backward-kill-word)
   ("M-RET" . selectrum-submit-exact-input))
  :custom
  (selectrum-fix-minibuffer-height t)
  (selectrum-num-candidates-displayed 8)
  ;; :custom-face
  ;; (selectrum-current-candidate ((t (:background "#D8DEE9" :foreground "#3B4252"))))
  :init
  (selectrum-mode +1))


(use-package prescient
  :straight t
  :custom
  (prescient-filter-method '(literal regexp fuzzy))
  (prescient-history-length 1000)
  (prescient-save-file
   (expand-file-name "prescient-save.el"
                     no-littering-var-directory))
  :config
  (prescient-persist-mode +1))


(use-package selectrum-prescient
  :straight t
  :demand t
  :after (selectrum prescient)
  :init
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1)
  :custom
  (prescient-filter-method '(literal regexp initialism)))


(use-package marginalia
  :after selectrum
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


(use-package corfu
  :straight (:type git
                   :host github
                   :repo "minad/corfu"
                   :branch "main"
                   :files (:defaults "extensions/*.el"))
  :ensure t
  :defer t
  :hook ((prog-mode . corfu-mode)
         (corfu-mode . corfu-history-mode))
  :bind (:map corfu-map
              ("C-q" . #'corfu-quick-insert)
              ("C-g" . #'corfu-quit)
              ("<return>" . #'corfu-insert))
  :custom
  (corfu-cycle nil)
  (corfu-auto t)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5))


(use-package corfu-doc
  :straight t
  :after corfu
  :custom
  (corfu-doc-auto nil)
  (corfu-doc-max-width 85)
  (corfu-doc-max-height 20)
  :bind (:map corfu-map
              ("M-d" . #'corfu-doc-toggle)
              ("M-p" . #'corfu-doc-scroll-down)
              ("M-n" . #'corfu-doc-scroll-up))
  :hook (corfu-mode . corfu-doc-mode))


(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; (use-package orderless
;;   :straight t
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-overrides '((file (styles basic partial-completion))))
;;   :custom
;;   (orderless-skip-highlighting (lambda () selectrum-is-active))
;;   (selectrum-highlight-candidates-function #'orderless-highlight-matches))


(use-package use-package-ensure-system-package
  :after use-package
  :straight t)


(use-package which-key
  :straight t
  :init
  (which-key-mode +1)
  :config
  (setq which-key-popup-type 'side-window))


(use-package treemacs
  :straight t
  :defer t
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
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("s-t"       . treemacs-display-current-project-exclusively)
        ("s-T"       . treemacs-add-and-display-current-project)
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package treemacs-icons-dired
  :straight t
  :after (treemacs)
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)


(use-package treemacs-magit
  :straight t
  :after (treemacs magit)
  :ensure t)


(use-package treemacs-all-the-icons
  :straight t
  :after treemacs)


(use-package whitespace
  :straight t
  :commands (whitespace-mode)
  :hook ((prog-mode . whitespace-mode)
         (text-mode . whitespace-mode)
         (before-save . whitespace-cleanup))
  :config
  (setq whitespace-line-column 115)
  (setq whitespace-style '(face tabs empty trailing lines-tail)))


(use-package multiple-cursors
  :defer t
  :straight t
  :bind
  (("C-M-s-. C-M-s-." . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
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
  :straight t
  :init
  (save-place-mode 1)
  :config
  (setq save-place-file
        (expand-file-name "saveplace" bg/save-dir))
  (setq-default save-place t))


(use-package savehist
  :straight t
  :init
  (savehist-mode 1)
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file
        (expand-file-name "savehist" bg/save-dir)))


(use-package recentf
  :straight nil
  :init
  (recentf-mode t)
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 50
        recentf-auto-cleanup 60)
  (add-to-list 'recentf-exclude bg/local-dir)
  (add-to-list 'recentf-exclude "/opt/homebrew/Cellar/"))


(use-package uniquify
  :straight nil
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


(use-package ace-window
  :straight t
  :init
  (global-set-key (kbd "M-o") 'ace-window))

(use-package avy
  :straight t
  :bind
  ("M-g M-c" . avy-goto-char-timer)
  ("M-g M-g" . avy-goto-line)
  :config
  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful))


;; (use-package mini-frame
;;   :straight t
;;   :custom
;;   (nconc '(ctrlf-forward-default
;;             ctrlf-forward-alternate
;;             ctrlf-backward-default
;;             ctrlf-backward-alternate)
;;           mini-frame-ignore-commands)
;;   :custom
;;   (mini-frame-show-parameters
;;    '((top . 90)
;;      (width . 0.7)
;;      (left . 0.5)))
;;   :init
;;   (mini-frame-mode 1))


;; (use-package popper
;;   :straight t
;;   :bind (("C-`"   . popper-toggle-latest)
;;          ("M-`"   . popper-cycle)
;;          ("C-M-`" . popper-toggle-type))
;;   :custom
;;   (popper-mode-line nil)
;;   (popper-group-function #'popper-group-by-projectile)
;;   :init
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           ("Output\\*$" . hide)
;;           (completion-list-mode . hide)
;;           "\\*Async Shell Command\\*"
;;           "\\*scratch\\*"
;;           "^\\*vterm.*\\*$"  vterm-mode
;;           help-mode
;;           compilation-mode))
;;   (popper-mode +1)
;;   (popper-echo-mode +1))


(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode)
  :config
  (defun kc/hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             ;; TODO: Fix this case. `hs-show-block' needs to be
             ;; called twice to open all folds of the parent
             ;; block.
             (save-excursion (hs-show-block))
             (hs-show-block)
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun kc/hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))
  :bind
  ("C-<tab>" . kc/hs-cycle)
  ("C-s-<tab>" . kc/hs-global-cycle))


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
  (setq pulsar-highlight-face 'pulsar-yellow)
  :bind
  ("C-x l" . #'pulsar-pulse-line-green)
  ("C-c h h" . #'pulsar-highlight-dwim)
  :hook
  ((next-error . #'pulsar-pulse-line)))


(use-package lin
  :straight t
  :config
  (customize-set-variable 'lin-face 'lin-yellow-override-fg))
