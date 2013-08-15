#lang racket

(provide serializable%)
 
(define current-id (box 1000))

(define serializable%
  (class object%
    (define/public (make-next)
      (+ 1 (unbox current-id)))
    
    ; to be augmented by subclasses
    (define/pubment (from-map map)
      (set-field! uid this (hash-ref map uid))
      (inner #f from-map map))

    (define/pubment (to-map)
      (hash-set! (inner #f to-map) '(('uid . uid))))

    (init-field [uid (make-next)])

    (super-new)
    
    (define (update-current)
      (when (> uid (unbox current-id))
          (set-box! current-id uid)))
    
    (update-current)))          

(module+ test
  (require rackunit)
  
  (define (set-id x)
    (set-box! current-id x))
  
  ; new objects get increasing uids
  (set-id 1000)
  (check-equal? (get-field uid (new serializable%)) (+ 1001))
  (check-equal? (get-field uid (new serializable%)) (+ 1002))
  
  ; setting the id to a lower value doesn't increase current-id
  (set-id 1000)
  (check-equal? (get-field uid (new serializable% [uid 1])) 1)
  (check-equal? (get-field uid (new serializable%)) (+ 1001))
  
  (set-id 1000))
  