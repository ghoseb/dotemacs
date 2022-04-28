;;; core.el

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(setq tab-always-indent 'complete)

(setq require-final-newline t)

(setq-default fill-column 115)

(delete-selection-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


(use-package selectrum
  :straight t
  :init
  (selectrum-mode +1)
  :config
  (global-set-key (kbd "C-x C-z") #'selectrum-repeat))

(use-package prescient
  :straight t
  :config (prescient-persist-mode +1))

(use-package selectrum-prescient
  :straight t
  :after selectrum
  :init
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

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
  :config (ctrlf-mode +1))

(use-package blackout
  :straight t
  :demand t)

(blackout 'auto-fill-mode)
(blackout 'eldoc-mode)
(blackout 'emacs-lisp-mode "EL")

(use-package company
  :straight t
  :config (add-hook 'prog-mode-hook 'company-mode)
  :bind (:map company-active-map
              ("<tab>" . #'company-complete-selection)
              ("TAB" . #'company-complete-selection)
              ("C-s" . nil)
              ([remap scroll-down-command] . nil)
              ([remap scroll-up-command] . nil))
  :config
  (setq company-idle-delay 0.15)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-minimum company-tooltip-limit)
  (setq company-frontends '(company-pseudo-tooltip-frontend))
  (setq company-show-quick-access t)
  (setq company-require-match #'company-explicit-action-p)
  (setq company-tooltip-align-annotations t))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :straight t
  :after (company prescient)
  :init (company-prescient-mode +1))

(use-package use-package-ensure-system-package
  :straight t)

(use-package which-key
  :defer t
  :straight t
  :config
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode +1))

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
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
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
    (treemacs-resize-icons 44)

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


(use-package whitespace
  :straight t
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
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
  :defer t
  :straight t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil))


(use-package saveplace
  :straight t
  :defer t
  :config
  (setq save-place-file
        (expand-file-name "saveplace" bg/save-dir))
  (setq-default save-place t))


(use-package savehist
  :straight t
  :defer t
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file
        (expand-file-name "savehist" bg/save-dir))
  (savehist-mode +1))


(use-package recentf
  :straight t
  :defer t
  :config
  (setq recentf-save-file
        (expand-file-name "recentf" bg/save-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode +1))


;; misc settings
(setq ring-bell-function #'ignore)
(setq echo-keystrokes 1e-6)
