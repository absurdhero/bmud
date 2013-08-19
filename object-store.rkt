#lang racket

(provide
 get-object
 add-object)

(define objects (make-hash))

(define (get-object id) (hash-ref objects id))

(define (add-object obj) (hash-set! objects (get-field uid obj) obj))

(require json)

(define (to-json serial-obj)
  (jsexpr->string (send serial-obj to-map)))

(define (from-json json-string serial-class)
  (define serial-obj (new serial-class))
  (send serial-obj from-map (string->jsexpr json-string)))
