;;; settings.el

(setq user-full-name "Baishampayan Ghose"
      user-mail-address "b.ghose@gmail.com")

(defvar bg--default-font "Berkeley Mono" "The default font.")
(defvar bg--variable-pitch-font "IBM Plex Sans" "The default variable pitch font.")
(defvar bg--fixed-pitch-font bg--default-font "The default fixed pitch font.")
(defvar bg--default-font-size 150 "The default font size.")

;; Let us enable a nice font for Emojis
(defvar bg--emoji-font "Noto Emoji" "The font for emojis.")

(defvar bg--mode-line-font "IBM Plex Sans Condensed" "The font for the Modeline")

(defvar bg--dark-theme 'kaolin-valley-dark "The dark theme.")
(defvar bg--light-theme 'kaolin-valley-light "The light theme.")

(defvar bg--default-theme bg--dark-theme "The default theme.")
(defvar bg--alternative-theme bg--light-theme "The alternative theme.")
