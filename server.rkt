#lang racket

(provide
 serve
 (struct-out server-control))

; monitor and stop a server
(struct server-control (thread stop))

; provide a port number and a function that will handle connections.
; connected-handler is given the input and output ports.
(define (serve port-no connected-handler)
  (define main-custodian (make-custodian))
  (parameterize ([current-custodian main-custodian])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener connected-handler)
      (loop))
    
    (define server-thread (thread loop))
    
    (displayln "started")
    
    ; return the thread and a function to stop the server and all connections
    (server-control server-thread
                    (lambda ()
                      (custodian-shutdown-all main-custodian)
                      (displayln "stopped")))))

(define (accept-and-handle listener connected-handler)
  (define cust (make-custodian))
  (custodian-limit-memory cust (* 10 1024 1024))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (file-stream-buffer-mode out 'none)
    (file-stream-buffer-mode in 'none)
    (thread
     (lambda ()
       (connected-handler in out)))))


; a sample connected-handler that echos lines of input to output indefinitely
(define (echo in out)
  (displayln (read-line in) out)
  (echo in out))
