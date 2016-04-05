# BG's dotemacs redux

A completely new approach based on `Cask` and `use-package`.

To install `Cask` on OS X run the following command:

    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

The code inside `init.el` expects `cask.el` to be located in `"~/.cask/cask.el"`.
If you've installed Cask in any other way then you may change the appropriate
line in `init.el`.

To start dotemacs setup on OS X run the following command `EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs" cask install`
(this is done to make sure you don't launch the ancient Emacs that's shipped with OS X).

Afterwards you may launch Emacs as usual.

Still a work in progress...

## Prerequisites

- Fonts
  - [Source Code Pro](https://github.com/adobe-fonts/source-code-pro/releases)
  - [Symbola](http://www.fonts2u.com/symbola.font) (for Emoji etc)
  - [Gentium Plus](http://software.sil.org/gentium/) (for Greek)

Your `~/.lein/profiles.clj` should at least have the following stuff:

```Clojure
{:user {:plugins [[cider/cider-nrepl "0.11.0"]
                  [refactor-nrepl "2.2.0"]]}}
```

## Getting started

To learn Emacs you should follow any generic Emacs tutorial. If you're
a Clojure programmer then you may follow the guide in
[Brave Clojure](http://www.braveclojure.com/basic-emacs/).
