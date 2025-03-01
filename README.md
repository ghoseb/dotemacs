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

Optimized for [Emacs 30+](https://github.com/jimeh/emacs-builds) on MacOS with native compilation and GC hacks.

*Startup time is ~400ms.*

```bash
# Installing Emacs on MacOS using Homebrew

brew tap jimeh/emacs-builds
brew install --cask emacs-app
```

## Screenshot

<p>
    <figure>
        <img src="/screenshots/pic2.png" alt="Screenshot of my GNU/Emacs setup." title="GNU/Emacs" />
        <figcaption>Theme: <a href="https://protesilaos.com/emacs/ef-themes">Ef Dream</a></figcaption>
    </figure>
</p>
