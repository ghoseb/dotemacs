# BG's Emacs Setup #

A fresh approach to managing my Emacs setup.
*Still a work in progress.*

## Philosophy ##

The main idea behind my new setup is to make it very easy to customize the
settings and add new libs.

There is no *magic* in here.

## Directory Structure ##

Everything is contained in a bunch of directories and `.el` files. The directory
structure is somewhat like this -

    ~/.emacs.d
        - init.el # the main entry point
        - config # config files for every mode
        - etc # additional file for modes
        - lib # place to put/symlink libs
        - vendor
          - checkouts # place to extract lib tarballs
          - submodules # place to put lib git submodules
        - tmp # temporary stuff

To understand how to add your own library take a look at `config/ac.el` and the
corresponding files in `lib` and `vendor/submodules`.

## Supported Languages ##

* Clojure
* <contributions welcome!>

## Acknowledgements ##

Based on the work of Sam Aaron.
