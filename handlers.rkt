#lang racket

; expose function to find a handler

(provide
 session-ctx%
 get-handler
 create-handler
 single-line-handler)

; session information passed to handlers
(define session-ctx%
  (class object%
    (init-field command args user out)
    
    (super-new)
    
    (define/public (print text) (displayln text out))))

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

(single-line-handler
 "hello"
 (lambda (session)
   (send session print "Hello, World!")))


(single-line-handler
 "walk"
 (lambda (session)
   (define args (get-field args session))
   (define user (get-field user session))
   (define (print text) (send session print text))
   
   (define (move direction)
     (define old-location (hash-ref user "location"))
     (define new-location
                (case direction
                  [(string "n") (cons (car old-location) (+ (cdr old-location) 1))]
                  [(string "s") (cons (car old-location) (- (cdr old-location) 1))]
                  ))
     (hash-set! user "location" new-location)
     new-location)
     
   (if (empty? args)
       (print "where do you want to walk?")
       (begin
         (move (first args))
         (print (string-join
                 (list "walking" (first args) "to" (~a (hash-ref user "location")))))
         ))))

