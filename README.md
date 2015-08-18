# BG's dotemacs redux

A completely new approach based on `Cask` and `use-package`.

To install `Cask` on OS X run the following command `brew install cask`

To start dotemacs setup on OS X run the following command `EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs" cask install`

Still a work in progress...

## Prerequisites

- Fonts
  - Source Code Pro
  - Symbola (for Emoji etc)
  - Gentium Plus (for Greek)

Your `~/.lein/profiles.clj` should at least have the following stuff:

    [[cider/cider-nrepl "0.9.1"]
     [refactor-nrepl "1.1.0"]]
