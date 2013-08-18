;;; https://github.com/lewang/flx
(live-add-pack-lib "flx-ido")
(require 'flx-ido)
(flx-ido-mode 1)
(setq flx-ido-threshhold 1000)          ;change it if you have a fast processor.
