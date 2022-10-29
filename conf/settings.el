;;; settings.el

(setq user-full-name "Baishampayan Ghose"
      user-mail-address "b.ghose@gmail.com")

(defvar bg--default-font "Cascadia Code" "The default font.")
(defvar bg--variable-pitch-font "IBM Plex Sans" "The default variable pitch font.")
(defvar bg--fixed-pitch-font bg--default-font "The default fixed pitch font.")
(defvar bg--default-font-size 160 "The default font size.")

;; Let us enable a nice font for Emojis
(defvar bg--emoji-font "Noto Emoji" "The for for emojis.")

(defvar bg--dark-theme 'kaolin-dark "The dark theme.")
(defvar bg--light-theme 'kaolin-light "The light theme.")

(defvar bg--default-theme bg--dark-theme "The default theme.")
(defvar bg--alternative-theme bg--light-theme "The alternative theme.")
