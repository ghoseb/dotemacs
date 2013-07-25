;;; SLIME for Common Lisp

(live-add-pack-lib "slime")
(require 'slime)
(slime-setup '(slime-fancy))

(setq inferior-lisp-program "/usr/local/bin/sbcl"
      slime-starup-animation nil
      lisp-indent-function 'common-lisp-indent-function)

(defmacro defslime-start (name lisp)
  `(defun ,name ()
     (interactive)
     (let ((inferior-lisp-program ,lisp))
       (slime))))

(defslime-start sbcl "/usr/local/bin/sbcl")
;; (defslime-start clozure "/usr/local/bin/ccl")

(let ((fasl-dir (expand-file-name "/tmp/slime-fasls/")))
  (make-directory fasl-dir t)
  (setq slime-compile-file-options `(:fasl-directory ,fasl-dir)))

(setq slime-protocol-version 'ignore)
(setq slime-net-coding-system 'utf-8-unix)

(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
(add-hook 'slime-mode-hook (lambda () (slime-autodoc-mode t)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'slime-connected-hook (lambda () (paredit-mode +1)))

(define-key slime-mode-map (kbd "C-M-t") 'transpose-chars)
(define-key slime-mode-map (kbd "C-t") 'transpose-sexps)
(define-key slime-mode-map (kbd "C-M-b") 'backward-char)
(define-key slime-mode-map (kbd "C-b") 'backward-sexp)
(define-key slime-mode-map (kbd "C-M-f") 'forward-char)
(define-key slime-mode-map (kbd "C-f") 'forward-sexp)
(define-key slime-mode-map (kbd "<f7>") 'vertical-split-slime-repl)
(define-key slime-mode-map (kbd "C-c h") 'slime-hyperspec-lookup)
;;  (define-key slime-mode-map (kbd "C-c C-d h") 'cl-lookup)
(define-key slime-mode-map (kbd "<C-S-iso-lefttab>") 'slime-fuzzy-complete-symbol)
(define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
(define-key slime-repl-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
(define-key slime-repl-mode-map (kbd "<C-S-iso-lefttab>") 'slime-fuzzy-complete-symbol)

(define-key slime-mode-map (kbd "<f5>") 'slime-selector)
(define-key slime-repl-mode-map (kbd "<f5>") 'slime-selector)
(define-key sldb-mode-map (kbd "<f5>") 'slime-selector)

;; Balanced comments
(define-key slime-mode-map (kbd "C-c ;") 'slime-insert-balanced-comments)
(define-key slime-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comment)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

(defun lispdoc ()
  "Searches lispdoc.com for SYMBOL, which is by default the symbol
currently under the curser"
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
                                                   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))

(define-key lisp-mode-map "C-c l" 'lispdoc)
