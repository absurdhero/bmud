#lang racket

(require "user.rkt")
(require "map.rkt")

(provide
 session-ctx%
 current-session)

; session information passed to handlers
(define session-ctx%
  (class object%
    (init-field command args user out)
    
    (super-new)
    
    (define/public (print text) (displayln text out))))

; must be set by caller using (parameterize)
(define current-session (make-parameter null))

; a handler optionally returns a function
; that handles the next line of input
(define (create-root-handler name fn)
  (send mud-root register-command name fn))

; prints handler output and returns false
; indicating that the handler is done
(define (single-line-handler name fn)
  (create-root-handler name fn)
  #f)

; command handlers

(single-line-handler
 "hello"
 (lambda (target)
   (send (current-session) print "Hello, World!")))

(single-line-handler
 "directions"
 (lambda (target)
   (define user (get-field user (current-session)))
   (send (current-session) print
         (string-join (send (send user room) directions)))))

(single-line-handler
 "walk"
 (lambda (target)
   (define args (get-field args (current-session)))
   (define user (get-field user (current-session)))
   (define (print text) (send (current-session) print text))
   
   (define (move direction)
     (define current-room (send user room))
     (if (send current-room connects? direction)
         (send user room-set! (send current-room go-to direction))
         #f))
   
   (if (empty? args)
       (print "where do you want to walk?")
       (begin
         (if (move (first args))
             (print (string-join
                 (list "walking" (first args) "to" (get-field name (send user room)))))
             (print "You can't go that way."))
         ))))

