#lang racket
(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(require "env.rkt")
(require "store.rkt")
(require "errors.rkt")

(define-datatype thunk thunk?
  (a-thunk
   (exp expression?)
   (saved-env environment?)))

(define-datatype proc proc?
  (a-proc
   (p-name string?)
   (params list?)
   (p-body statement?)))

(define (extend-env-with-functions env)
  (let loop ([env env]
             [g-env the-global-env])
    (cases environment g-env
      (empty-env-record (global) env)
      (extended-env-record (global var val next-env)
        (cases expression val
          (ref_val (ref)
            (let ([w (deref ref)])
              (if (proc? w)
                  (cases proc w
                    (a-proc (p-name params p-body)
                        (loop (extend-env p-name val env) next-env)))
                  (loop env next-env))
                ))
          (else
            (report-type-error 'extend-env-with-functions)))))))

(provide (all-defined-out))
(#%provide (all-defined))