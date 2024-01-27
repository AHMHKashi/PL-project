#lang racket

(require "errors.rkt")
(provide (all-defined-out))

(define the-store 'uninitialized)

(define get-store
  (lambda () the-store))

(define (empty-store) '())

(define (initialize-store!)
  (set! the-store (empty-store)))

(define (newref val)
  (let ((next-ref (length the-store)))
    (set! the-store (append the-store (list val)))
    next-ref))

(define (deref ref)
  (list-ref the-store ref))
