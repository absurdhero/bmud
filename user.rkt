#lang racket

(provide
 user-exists?
 user-get
 create-user)

(define users (make-hash))

(define (user-exists? name)
  (hash-has-key? users name))

(define (user-get name)
  (hash-ref users name))

(define (create-user name)
  (define state (make-hash))
  (hash-set! state "name" name)
  (hash-set! state "location" (cons 0 0))
  (hash-set! users name state))
