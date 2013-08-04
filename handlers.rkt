#lang racket

; expose function to find a handler

(provide get-handler session-ctx)

; session information passed to handlers
(struct session-ctx (command args print))

(define dispatch-table (make-hash))

(define (get-handler name)
  (hash-ref dispatch-table name #f))

; a handler optionally returns a function
; that handles the next line of input
(define (create-handler name fn)
  (hash-set! dispatch-table name fn))

; prints handler output and returns false
; indicating that the handler is done
(define (single-line-handler name fn)
  (create-handler name fn)
  #f)

; command handlers

(single-line-handler "hello"
           (lambda (session)
             ((session-ctx-print session) "Hello, World!")))

(single-line-handler "walk"
           (lambda (session)
             (define args (session-ctx-args session))
             (define print (session-ctx-print session))

             (if (empty? args)
                 (print "where do you want to walk?")
                 (print (string-join
                         (list "walking" (first (session-ctx-args session))))))))

