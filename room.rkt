#lang racket

(require "serializable.rkt")

(provide room%
         passage%
         in-room-mixin
         room-dispatch-interface
         serializable-in-room%)

(define room-dispatch-interface
  (interface ()
    invoke-command
    register-command
    set-outer))

; A mixin that supports executing registered commands or dispatching them to
; the containing room.

(define in-room-mixin
  (mixin () (room-dispatch-interface)
    (super-new)
    
    (field (outer-room #f) (commands (make-hash)))
    
    (define/public (register-command cmd func)
      (unless (eq? (procedure-arity func) 1)
        (raise-arity-error func 1))
      (log-debug (~a "registered command " cmd " in " (get-field uid this)))
      (hash-set! commands cmd func))
    
    (define/public (invoke-command cmd)
      (log-debug (~a cmd " invoked in " (get-field uid this)))
      (if (hash-has-key? commands cmd)
          ((hash-ref commands cmd) this)
          (if outer-room
              (send outer-room invoke-command cmd)
              (raise-user-error "command not found"))))
    
    (define/public (set-outer room)
      (log-debug (~a (get-field uid this) " setting outer room to " (get-field uid room)))
      (set! outer-room room))
    ))

(define serializable-in-room%
  (in-room-mixin
   (class serializable%
     (super-new)
     
     (define/augment (from-map map)
       (send this set-outer (hash-ref map 'outer-room))
       (inner #f from-map map))
     
     (define/augment (to-map)
       (hash-set! (inner #f to-map)
                  (list (cons 'outer-room (get-field outer-room this)))))
     )))

(define room%
  (class serializable-in-room%
    (inherit make-next)
    (init-field [name #f])
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
    (define/public (has-prop? ref) (hash-has-key? properties ref))
    
    ; serialization
    (define/augment (from-map map)
      (set-field! name this (hash-ref map 'name))
      (set-field! connections this (hash-ref map 'connections))
      (set-field! properties this (hash-ref map 'properties)))
    
    (define/augment (to-map)
      (hash-set*! (make-hash) 'name name 'connections connections 'properties properties))
    ))

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
  
  (define in-a-room%
    (in-room-mixin (class serializable% (super-new))))
  
  
  (define obj-in-room (new in-a-room%))
  (define outer-room (new in-a-room%))
  
  ; called for each test that uses the above room objects
  (define (reset-rooms)
    (set! obj-in-room (new in-a-room%))
    (set! outer-room (new in-a-room%))
    (send obj-in-room set-outer outer-room))
  
  (define room-tests
    (test-suite
     "given rooms"
     
     (test-case "cannot go north when room does not connect"
                (check-equal? (send single-room connects? (get-field to north)) #f))
     
     (test-case "can move back and forth when two rooms are connected"
                (define other-room (new room% (name "other")))
                (send single-room connect other-room north)
                
                (check-equal? (send single-room connects? north) #t)
                (check-equal? (send single-room connects? "n") #t)
                
                (check-equal? (send other-room connects? (get-field reverse north)) #t)
                (check-equal? (send single-room go-to north) other-room)
                )
     
     (test-case "raise an exception when a command does not exist in a single room"
                (define obj-in-room (new in-a-room%))
                (check-exn exn:fail:user?
                           (λ ()
                             (send obj-in-room invoke-command "test"))))
     
     (test-suite
      "given an object inside of an outer room"
      
      (test-case "raise an exception when a command does not exist"
                 (reset-rooms)
                 (check-exn exn:fail:user?
                            (λ ()
                              (send obj-in-room invoke-command "test"))))
      
      (test-case "run a command when it is invoked"
                 (reset-rooms)
                 (define was-invoked #f)
                 (send obj-in-room register-command "test"
                       (lambda (target) (set! was-invoked #t)))
                 
                 (check-not-exn
                  (λ () (send obj-in-room invoke-command "test")))
                 (check-equal? was-invoked #t "expected command to be invoked"))
      
      (test-case "the most specific command runs when there are multiple implementations"
                 (reset-rooms)
                 
                 (send obj-in-room register-command "test"
                       (lambda (target) (set! was-invoked #t)))
                 
                 (define was-invoked #f)
                 
                 (send outer-room register-command "test"
                       (lambda (target) (set! was-invoked #f)))
                 
                 (check-not-exn
                  (λ () (send obj-in-room invoke-command "test")))
                 (check-equal? was-invoked #t "expected command to be invoked"))
      
      (test-case "command defined in outer room is run when invoked from inner"
                 (reset-rooms)
                 (define was-invoked #f)
                 
                 (send outer-room register-command "test"
                       (lambda (target) (set! was-invoked #t)))
                 
                 (check-not-exn
                  (λ () (send obj-in-room invoke-command "test")))
                 (check-equal? was-invoked #t "expected command to be invoked"))
      
      (test-case "command defined two levels up is run when invoked from inner-most room"
                 (reset-rooms)
                 
                 (define was-invoked #f)
                 
                 (send outer-room register-command "test"
                       (lambda (target) (set! was-invoked #t)))
                 
                 (define inner-inner-room (new in-a-room%))
                 (send inner-inner-room set-outer obj-in-room)
                 
                 (check-not-exn
                  (λ () (send inner-inner-room invoke-command "test")))
                 (check-equal? was-invoked #t "expected command to be invoked"))
      ))
    )
  
  (require rackunit/text-ui)
  (run-tests room-tests)
  )
