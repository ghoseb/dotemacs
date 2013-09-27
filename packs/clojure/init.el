;; Clojure Pack

(require 'rainbow-delimiters)

(live-load-config-file "lisps-conf.el")
(live-load-config-file "highlight-flash-conf.el")
(live-load-config-file "clojure-conf.el")
;; (live-load-config-file "slime-conf.el") ; WILL CONFLICT WITH CL'S SLIME. ENABLE THIS AT YOUR OWN RISK
(live-load-config-file "auto-complete-conf.el")
(live-load-config-file "nrepl-conf.el")
