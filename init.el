;; This is where everything starts

;; Create a variable to store the path to this dotfile directory
;; (Usually ~/.emacs.d)
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Create variables to store the path to this dotfile dir's lib etc and tmp directories
(setq dotfiles-lib-dir (concat dotfiles-dir "lib/"))
(setq dotfiles-tmp-dir (concat dotfiles-dir "tmp/"))
(setq dotfiles-etc-dir (concat dotfiles-dir "etc/"))

;; Create helper fns for loading dotfile paths and files
(defun add-dotfile-path (p)
  (add-to-list 'load-path (concat dotfiles-dir p)))

(defun add-lib-path (p)
  (add-to-list 'load-path (concat dotfiles-lib-dir p)))

(defun load-dotfile (f)
  (load-file (concat dotfiles-dir f)))

;; Ensure the lib directory is on the load path
(add-dotfile-path "lib")

;; Pull in personalised configuration files
(load-dotfile "config/core.el")
(load-dotfile "config/utils.el")
(load-dotfile "config/cosmetic.el")
(load-dotfile "config/clojure.el")
(load-dotfile "config/built-in.el")
(load-dotfile "config/ido.el")
(load-dotfile "config/paredit.el")
(load-dotfile "config/lisp.el")
(load-dotfile "config/slime.el")
(load-dotfile "config/durendal.el")
(load-dotfile "config/ac.el")
(load-dotfile "config/smex.el")
(load-dotfile "config/undo-tree.el")
(load-dotfile "config/yas.el")
(load-dotfile "config/bindings.el")
