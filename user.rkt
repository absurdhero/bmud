#lang racket

(require "serializable.rkt")


(provide
 user-exists?
 user-get
 create-user)

(define users (make-hash))

(define (user-exists? name)
  (hash-has-key? users name))

(define (user-get name)
  (hash-ref users name))

(define (create-user name starting-room)
  (hash-set! users name
             (new user% (name name) (starting-room starting-room))))


(define user%
  (class serializable%
  (inherit make-next)
  (init-field [name #f] [starting-room #f])
  (init [uid (make-next)])
  (super-new [uid uid])
  
  (define properties (make-hash))
  
  (define/augment (from-map map)
    (set-field! name this (hash-ref map 'name))
    (set-field! properties this (hash-ref map 'properties)))
    
  (define/augment (to-map)
    (hash-set*! (make-hash) 'name name 'properties properties))
  
  (prop-set! "room" starting-room)
  (send starting-room user-join this)
  
  (define/public (prop ref)
    (hash-ref properties ref))
  
  (define/public (prop-set! ref value)
    (hash-set! properties ref value))
  
  (define/public (room)
    (prop "room"))
  
  (define/public (room-set! next-room)
    (send (room) user-leave this)
    (send next-room user-join this)
    (prop-set! "room" next-room)
    )))
