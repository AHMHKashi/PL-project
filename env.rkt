#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(require "errors.rkt")

(provide (all-defined-out))

(define-datatype environment environment?
  (empty-env-record
    (global boolean?))
  (extended-env-record
    (global boolean?)
    (var string?)
    (val expression?)
    (next-env environment?)))


(define (apply-env search-var env)
  (cases environment env
    (empty-env-record (global)
        (atomic_null_exp))
    (extended-env-record (global saved-var saved-val next-env)
      (if (equal? saved-var search-var)
        saved-val
        (apply-env search-var next-env)))
    (else
      (report-invalid-env env)))
)

(define empty-env empty-env-record)

(define (extend-env var val env)
  (cases environment env
    (empty-env-record (global)
      (extended-env-record global var val env))
    (extended-env-record (global next-var next-val next-env)
      (extended-env-record global var val env))))



(define the-global-env 'uninitialized)
(define the-scope-env 'uninitialized)

(define (initialize-global-env!)
  (set! the-global-env (empty-env #t)))

(define (initialize-scope-env!)
  (set! the-scope-env (empty-env #t)))

(define (update-scope-env! env)
  (set! the-scope-env env))

(define (update-global-env! env)
  (set! the-global-env env))

  (define (global-scope? env)
  (cases environment env
    (empty-env-record (global)
      global)
    (extended-env-record (global var val next-env)
      global)))