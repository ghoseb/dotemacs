;;; startup.rkt -- entry point

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Apr 25, 2009 22:36

;;; Code:

(require version/utils)
(unless (version<=? "5.0" (version))
  (error 'geiser
         "Racket version 5.0 or better required (found ~a)"
         (version)))

(require errortrace)
(require geiser/user)

(init-geiser-repl)
