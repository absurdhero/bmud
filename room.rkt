#lang racket

(require "serializable.rkt")

(provide room% passage%)

(define room%
  (class serializable%
    (inherit make-next)
    (init-field name)
    (init [uid (make-next)])
    (super-new [uid uid])
    
    ; connect rooms together
    (define connections (make-hash))
    
    (define/public (connect room passage)
      (connect-one-way room passage)
      (send room connect-one-way this (get-field reverse passage)))

    (define/public (connect-one-way room passage)
      (hash-set! connections (get-field to passage) room))
    
    (define/public (connects? passage)
      (hash-has-key? connections
                     (to-direction passage)))
    
    (define/public (directions)
      (hash-keys connections))
    
    (define/public (go-to passage)
      (hash-ref connections (to-direction passage)))
    
    (define (to-direction passage)
      (if (string? passage)
                         passage
                         (get-field to passage)))

    ; track who is in the room
    (define users (make-hash))
    (define/public (user-join user)
      (hash-set! users (get-field uid user) user))
    (define/public (user-leave user)
      (hash-remove! users (get-field uid user)))

    ; store arbitrary properties
    (define properties (make-hash))
    (define/public (prop ref) (hash-ref properties ref))
    (define/public (prop-set! ref value) (hash-set! properties ref value))
    (define/public (has-prop? ref) (hash-has-key? properties ref))))

(define passage%
  (class object%
    (define (make-reverse)
      (new passage% (to fro-name) (fro to-name) (reverse this)))
    
    (init-field [(to-name to)] [(fro-name fro)] [reverse (make-reverse)])
    
    (super-new)))

(module+ test
  (require rackunit)
  
  (define single-room (new room% (name "single")))
  (define north (new passage% (to "n") (fro "s")))
  
  (check-equal? (send single-room connects? (get-field to north)) #f)

  (define other-room (new room% (name "other")))
  (send single-room connect other-room north)
  
  (check-equal? (send single-room connects? north) #t)
    (check-equal? (send single-room connects? "n") #t)

  (check-equal? (send other-room connects? (get-field reverse north)) #t)
  (check-equal? (send single-room go-to north) other-room)
  )
