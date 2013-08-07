#lang racket

(provide
 user-exists?
 user-get
 create-user
 user-prop
 user-prop-set!)

(define users (make-hash))

(define (user-exists? name)
  (hash-has-key? users name))

(define (user-get name)
  (hash-ref users name))

(define (create-user name)
  (define user (make-hash))
  (hash-set! user "name" name)
  (hash-set! user "location" (cons 0 0))
  (hash-set! users name user))

(define (user-prop user ref)
  (hash-ref user ref))

(define (user-prop-set! user ref value)
  (hash-set! user ref value))
