;;; settings.el

(setq user-full-name "Baishampayan Ghose"
      user-mail-address "b.ghose@gmail.com")

(setq bg/default-font "Monaco")
(setq bg/fixed-pitch-font "Monaco")
(setq bg/variable-pitch-font "Gill Sans")
(setq bg/default-font-size 140)

;; Let us enable a nice font for Emojis
(setq bg/emoji-font "Apple Color Emoji")

(setq bg/save-dir (expand-file-name ".local/save" user-emacs-directory))

(setq bg/default-theme 'doom-dracula)
(setq bg/alternative-theme 'doom-nord-light)