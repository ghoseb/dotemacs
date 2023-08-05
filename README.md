# BG's dotemacs straight

A next-generation [GNU/Emacs](https://www.gnu.org/software/emacs/) setup for hackers with deadlines.

## Key components

* [radian-software/straight.el](https://github.com/radian-software/straight.el) for package management.
* [minad/vertico](https://github.com/minad/vertico) for interactive completion.
* [radian-software/prescient](https://github.com/radian-software/prescient.el) for sorting and filtering.
* [minad/corfu](https://github.com/minad/corfu) for completion overlay.
* [radian-software/ctrlf](https://github.com/radian-software/ctrlf) for buffer text search.
* [project.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el) for project management.
* [joaotavora/eglot](https://github.com/joaotavora/eglot) for LSP.

Optimized for [Emacs 29+](https://github.com/d12frosted/emacs-plus) on MacOS with native compilation and GC hacks.

*Startup time is ~500ms.*

```bash
# Installing Emacs on MacOS using Homebrew

brew tap d12frosted/emacs-plus
brew install emacs-plus@29 --with-native-comp --with-modern-papirus-icon
```

## Screenshots

<p>
    <figure>
        <img src="/screenshots/pic1.png" alt="Screenshot of my GNU/Emacs setup." title="GNU/Emacs" />
        <figcaption>Theme: <a href="https://github.com/ogdenwebb/emacs-kaolin-themes">Kaolin Valley Dark</a></figcaption>
    </figure>
</p>
