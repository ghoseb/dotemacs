;;; settings.el

(setq user-full-name "Baishampayan Ghose"
      user-mail-address "b.ghose@gmail.com")

(setq bg/default-font "Cascadia Code")
(setq bg/variable-pitch-font "IBM Plex Sans")
(setq bg/fixed-pitch-font bg/default-font)
(setq bg/default-font-size 160)

;; Let us enable a nice font for Emojis
(setq bg/emoji-font "Noto Emoji")

(setq bg/dark-theme 'kaolin-dark)
(setq bg/light-theme 'kaolin-light)

(setq bg/default-theme bg/dark-theme
      bg/alternative-theme bg/light-theme)
