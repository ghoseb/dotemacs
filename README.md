# BG's Emacs Setup #
A fresh approach to managing my Emacs setup.

*Still a work in progress.*

## Philosphy ##
* No magic
* Simple
* Extensible
* Well documented (lol!)

## Directory Structure ##
    ~/.emacs.d
        - etc # auxilliary files, snippets, etc.
        - packs # different library/config "packs"
        - init.el # the main entry point

### Structure of Packs ###
Every *pack* is a logical group of Emacs Lisp libraries and
corresponding configuration files. Each pack is designed to be
completely stand-alone except certain functions to load the config files
from the correct directory which are provided by the `init.el` file.

Each pack must have this structure -

    my-pack/
        - lib/ # emacs Lisp library files
        - config/ # config files for each library as needed
        - init.el # entry point for the pack

Take a look at the `notes` pack for more information.

## Initialisation ##
All the packs are loaded from the top-level `init.el` file so you can
enable/disable loading of packs from there. Look at the bottom of the
said file for more information.


## Included Packs ##
* Core
  - core Emacs configuration
* Power
  - configuration for power-users
* Programming
  - settings for different programming languages
* Themes
  - colour theme settings
* Notes
  - note-taking using Deft & Org
* Clojure
  - for Clojure programming
* User
  - a special pack for user-level customizations
  - always loaded last

## Supported Programming Languages ##
* Clojure
* Python
* Go
* Haskell
* JavaScript
* HTML/CSS/SCSS

## Contributing ##
I welcome any contribution to the project in the form of patches, etc. I
am generally looking for any sort of configuration/tweak that is useful
for Emacs beginners and hackers alike.

Even though I may choose to not include something as default, I am fine
with keeping them when commented out/disabled by default.

If you have a patch, please send me a pull-request and then we can
discuss.

In case of any questions, feel free to email me - `b.ghose @ freegeek.in`.

## License ##
Copyright belongs to the respective authors. Everything else is in the
Public Domain.

## Acknowledgements ##
Based on the work of [Sam Aaron](https://github.com/overtone/emacs-live).
