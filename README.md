# BG's dotemacs redux

Clone the repository to `~/.emacs.d`.

Afterwards you may launch Emacs as usual. It will take some time to start up
the first time, so hang tight.

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
