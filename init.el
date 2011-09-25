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

(defun load-config (m)
  (load-dotfile (concat "config/" m ".el")))

;; Ensure the lib directory is on the load path
(add-dotfile-path "lib")

;; Pull in personalised configuration files
(load-config "core")
(load-config "utils")
(load-config "cosmetic")
(load-config "clojure")
(load-config "built-in")
(load-config "ido")
(load-config "paredit")
(load-config "lisp")
(load-config "slime")
(load-config "durendal")
(load-config "ac")
(load-config "smex")
(load-config "undo-tree")
(load-config "yas")
(load-config "bindings")
(load-config "git")
(load-config "markdown")
