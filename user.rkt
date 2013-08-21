#lang racket

(require "serializable.rkt")
(require "room.rkt")
(require "object-store.rkt")

(provide
 user-exists?
 user-id
 get-user
 create-user)

(define user-ids (make-hash))

(define (user-exists? name)
  (hash-has-key? user-ids name))

(define (user-id name)
  (hash-ref user-ids name))

(define (get-user name)
  (get-object (user-id name)))

(define (create-user name starting-room)
  (define user (new user% (name name) (starting-room starting-room)))
  (add-object user)
  (hash-set! user-ids name (get-field uid user))
  user)


(define user%
  (class serializable-in-room%
    (inherit make-next)
    (init-field [name #f] [starting-room #f])
    (init [uid (make-next)])
    (super-new [uid uid])
    
    (define properties (make-hash))
    
    (inherit set-outer)
    (set-outer starting-room)
    (send starting-room user-join this)
    
    (log-debug (~a "created user " name " #" uid))
    
    (define/augment (from-map map)
      (set-field! name this (hash-ref map 'name))
      (set-field! properties this (hash-ref map 'properties)))
    
    (define/augment (to-map)
      (hash-set*! (make-hash) 'name name 'properties properties))
    
    (define/public (prop ref)
      (hash-ref properties ref))
    
    (define/public (prop-set! ref value)
      (hash-set! properties ref value))
    
    (define/public (room)
      (get-field outer-room this))
    
    (define/public (room-set! next-room)
      (log-debug (~a "room set to " (get-field uid (room)) " for user " (get-field uid this)))
      (send (room) user-leave this)
      (send next-room user-join this)
      (set-outer next-room)
      )
    ))
