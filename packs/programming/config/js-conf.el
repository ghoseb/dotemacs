(live-add-pack-lib "js2")
(require 'js2-mode)


(defun turn-on-paredit ()
  (paredit-mode t))

(add-hook 'js2-mode-hook 'turn-on-paredit)

(defun fix-tab-width-for-js2-mode ()
  (setq-default tab-width 2)
  (setq js2-basic-offset 2))

(add-hook 'js2-mode-hook 'fix-tab-width-for-js2-mode)

(eval-after-load 'js2-mode
  '(progn (define-key js2-mode-map "{" 'paredit-open-curly)
          (define-key js2-mode-map "}" 'paredit-close-curly-and-newline)
          ;; fixes problem with pretty function font-lock
          (define-key js2-mode-map (kbd ",") 'self-insert-command)
          (font-lock-add-keywords
           'js2-mode `(("\\(function *\\)("
                        (0 (progn (compose-region (match-beginning 1)
                                                  (match-end 1) "ƒ")
                                  nil)))))))

(defun js-mode-changes ()
  (paredit-mode t)
  (setq js-indent-level 2)
  (define-key js-mode-map "{" 'paredit-open-curly)
  (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
  ;; fixes problem with pretty function font-lock
  (define-key js-mode-map (kbd ",") 'self-insert-command)
  (font-lock-add-keywords
   'js-mode `(("\\(function *\\)("
               (0 (progn (compose-region (match-beginning 1)
                                         (match-end 1) "ƒ")
                         nil))))))

(add-hook 'js-mode-hook 'js-mode-changes)

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
