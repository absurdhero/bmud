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
    (file-stream-buffer-mode out 'none)
    (file-stream-buffer-mode in 'none)
    (thread
     (lambda()
       (print-banner out)
       (handle in out)
       (close-input-port in)
       (close-output-port out)))))

(define (empty-trimmed-string? str)
      (zero? (string-length (string-trim str))))

(define (color-value color-name)
  (number->string
   (case color-name
     [('black) 30]
     ['red 31]
     ['green 32]
     ['yellow 33]
     ['blue 34]
     ['magenta 35]
     ['cyan 36]
     ['white 37])))

(define (colorize color text)
  (string-append (bytes->string/utf-8 #"\033\133") (color-value color) "m"
                 text
                 (bytes->string/utf-8 #"\033\1330m")))

(define (print-banner out)
  (display (colorize 'yellow "=== ") out)
  (display (colorize 'cyan "WELCOME") out)
  (displayln (colorize 'yellow " ===") out))

(define (print-prompt out)
  (display (colorize 'red ">") out)
  (display " " out))

(define (handle in out)
  ; extract command and arguments
  
  (print-prompt out)
  (define line (read-line in))
  (displayln line)
  
  (cond
    [(eof-object? line) #f]
    [(equal? (string-trim line) "exit")
     (displayln "goodbye" out)]
    [(empty-trimmed-string? line) (handle in out)]
    [else
     (if current-handler
         (current-handler line)
         (next-command line out)
         )
     (handle in out)]))

(define current-handler #f)

(define (next-command line out)
  (define words (string-split line))
  (define command (first words))
  (define (print text) (displayln text out))
  (define session (session-ctx command
                               (rest words)
                               print))
  (define handler (get-handler command))
  
  ; TODO return current-handler from handler
  (if handler
      (handler session)
      (begin
        (print "I don't understand")
        #f)))
