;;; enter.rkt -- custom module loaders

;; Copyright (C) 2010, 2012 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Wed Mar 31, 2010 21:53

#lang racket/base

(require syntax/modcode
         (for-syntax racket/base)
         racket/path)

(provide get-namespace enter-module module-loader module-loaded?)

(struct mod (name load-path timestamp depends))

(define (make-mod name path ts code)
  (let ([deps (if code
                  (apply append (map cdr (module-compiled-imports code)))
                  null)])
    (mod name (path->string path) ts deps)))

(define loaded (make-hash))

(define (module-loaded? path)
  (with-handlers ([exn? (lambda (_) #f)])
    (let ([rp (module-path-index-resolve (module-path-index-join path #f))])
      (hash-has-key? loaded (resolved-module-path-name rp)))))

(define (enter-module mod)
  (dynamic-require mod #f)
  (check-latest mod))

(define (module-loader orig)
  (enter-load/use-compiled orig #f))

(define inhibit-eval (make-parameter #f))

(define (get-namespace mod)
  (let ([mod (cond [(symbol? mod) mod]
                   [(string? mod) (find-module! (string->path mod) mod)]
                   [(path? mod) (find-module! mod (path->string mod))]
                   [else mod])])
    (and mod
         (with-handlers ([exn? (lambda (_) #f)])
           (parameterize ([inhibit-eval #t])
             (module->namespace mod))))))

(define (find-module! path path-str)
  (let ([m (or (hash-ref loaded path #f)
               (let loop ([ps (remove path (resolve-paths path))]
                          [seen '()])
                 (cond [(null? ps) #f]
                       [(hash-ref loaded (car ps) #f) =>
                        (lambda (m)
                          (add-paths! m (cdr ps))
                          (add-paths! m (cons path seen))
                          m)]
                       [else (loop (cdr ps) (cons (car ps) seen))])))])
    (list 'file (or (and m (mod-load-path m)) path-str))))

(define (add-paths! m ps)
  (for-each (lambda (p) (hash-set! loaded p m)) ps))

(define (resolve-paths path)
  (define (find root rest)
    (let* ([alt-root (resolve-path root)]
           [same? (equal? root alt-root)])
      (cond [(null? rest) (cons root (if same? '() `(,alt-root)))]
            [else (let* ([c (car rest)]
                         [cs (cdr rest)]
                         [rps (find (build-path root c) cs)])
                    (if same?
                        rps
                        (append rps (find (build-path alt-root c) cs))))])))
  (let ([cmps (explode-path path)])
    (find (car cmps) (cdr cmps))))

(define (notify re? path)
  (when re? (fprintf (current-error-port) " [re-loading ~a]\n" path)))

(define ((enter-load/use-compiled orig re?) path name)
  (when (inhibit-eval)
    (raise (make-exn:fail "namespace not found" (current-continuation-marks))))
  ;; (printf "Loading ~s: ~s~%" name path)
  (if (and name (not (list? name)))
      ;; Module load:
      (let* ([code (get-module-code
                    path "compiled"
                    (lambda (e)
                      (parameterize ([compile-enforce-module-constants #f])
                        (compile e)))
                    (lambda (ext loader?) (load-extension ext) #f)
                    #:notify (lambda (chosen) (notify re? chosen)))]
             [dir (or (current-load-relative-directory) (current-directory))]
             [path (path->complete-path path dir)]
             [path (normal-case-path (simplify-path path))])
        (define-values (ts real-path) (get-timestamp path))
        (add-paths! (make-mod name path ts code) (resolve-paths path))
        (parameterize ([current-module-declare-source real-path]) (eval code)))
      ;; Not a module:
      (begin (notify re? path) (orig path name))))

(define (get-timestamp path)
  (let ([ts (file-or-directory-modify-seconds path #f (lambda () #f))])
    (if ts
        (values ts path)
        (if (regexp-match? #rx#"[.]rkt$" (path->bytes path))
            (let* ([alt-path (path-replace-suffix path #".ss")]
                   [ts (file-or-directory-modify-seconds alt-path
                                                         #f
                                                         (lambda () #f))])
              (if ts
                  (values ts alt-path)
                  (values -inf.0 path)))
            (values -inf.0 path)))))

(define orig (current-load/use-compiled))

(define (check-latest mod)
  (define mpi (module-path-index-join mod #f))
  (define done (make-hash))
  (let loop ([mpi mpi])
    (define rpath (module-path-index-resolve mpi))
    (define path (let ([p (resolved-module-path-name rpath)])
                   (if (pair? p) (car p) p)))
    (when (path? path)
      (define npath (normal-case-path path))
      (unless (hash-ref done npath #f)
        (hash-set! done npath #t)
        (define mod (hash-ref loaded npath #f))
        (when mod
          (for-each loop (mod-depends mod))
          (define-values (ts actual-path) (get-timestamp npath))
          (when (ts . > . (mod-timestamp mod))
            (define orig (current-load/use-compiled))
            (parameterize ([current-load/use-compiled
                            (enter-load/use-compiled orig #f)]
                           [current-module-declare-name rpath]
                           [current-module-declare-source actual-path])
              ((enter-load/use-compiled orig #t) npath (mod-name mod)))))))))
