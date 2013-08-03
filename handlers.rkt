#lang racket

; expose function to find a handler

(provide get-handler)

(define dispatch-table (make-hash))

(define (get-handler name)
  (hash-ref dispatch-table name #f))

(define (create-handler name fn)
  (hash-set! dispatch-table name fn))

; command handlers

(create-handler "hello"
           (lambda (args)
             "Hello, World!"))
