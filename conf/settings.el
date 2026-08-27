;;; settings.el  -*- lexical-binding: t; -*-

(setq user-full-name "Baishampayan Ghose"
      user-mail-address "b.ghose@gmail.com")

(defvar bg--default-font "Monaspace Neon Frozen" "The default font.")
(defvar bg--variable-pitch-font "IBM Plex Sans" "The default variable pitch font.")
(defvar bg--fixed-pitch-font bg--default-font "The default fixed pitch font.")

(defvar bg--monaspace-comment-font "Monaspace Argon Frozen"
  "Humanist variant for comments, doc strings, and italic emphasis.")
(defvar bg--monaspace-string-font "Monaspace Xenon Frozen"
  "Slab serif variant for string literals.")
(defvar bg--monaspace-keyword-font "Monaspace Krypton Frozen"
  "Industrial variant for keywords, builtins, and types.")

;; Nerd Font icons — symbols-only font, neutral metrics, no text glyphs.
(defvar bg--nerd-font "Symbols Nerd Font Mono" "Nerd Font fallback for icon glyphs.")

;; Emoji font.
(defvar bg--emoji-font "Noto Emoji" "The font for emojis.")

(defvar bg--mode-line-font bg--monaspace-keyword-font "The font for the mode-line.")
