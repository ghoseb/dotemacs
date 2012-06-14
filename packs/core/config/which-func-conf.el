(require 'which-func)
(which-func-mode 1)

;;; enable which-func mode for org & clojure modes
(mapcar (lambda (m) (add-to-list 'which-func-modes m))
        '(org-mode
          clojure-mode))

;;; uncomment to show the function in the header-line instead
;; (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
;;
;; (setq which-func-header-line-format
;;       '(which-func-mode
;;         ("" which-func-format)))
;;
;; (defadvice which-func-ff-hook (after header-line activate)
;;   (when which-func-mode
;;     (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
;;     (setq header-line-format which-func-header-line-format)))
