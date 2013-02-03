(setq user-full-name "Baishampayan Ghose")
(setq user-mail-address "b.ghose@helpshift.com")

;;; change this according to your needs
(defvar default-font "Anonymous Pro-14" "My default Emacs font.")

(set-frame-font default-font nil t)

(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
