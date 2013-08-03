#lang racket

(require "handlers.rkt")

(define (serve port-no)
  (define main-custodian (make-custodian))
  (parameterize ([current-custodian main-custodian])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))

  (displayln "started")
  
  ; return a function to stop the server and all client connections
  (lambda ()
    (custodian-shutdown-all main-custodian)
    (displayln "stopped")))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (custodian-limit-memory cust (* 10 1024 1024))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))    
    (thread
     (lambda()
       (handle in out)
       (close-input-port in)
       (close-output-port out)))))

(define (handle in out)
    ; extract command and arguments
  (define words (string-split (read-line in)))
  (define command (first words))
  (define args (rest words))

  (displayln (string-join words))
  
  (displayln (dispatch command args) out))

(define (dispatch command args)
  (define handler (get-handler command))

  (if handler
      (begin
        (handler args))
      '"I don't understand"))
