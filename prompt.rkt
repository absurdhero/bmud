#lang racket

(require "handlers.rkt")

(provide new-mud-connection)

; serves clients over an input and output port
(define (new-mud-connection in out)
  (print-banner out)
  (next-line in out) ;; processes input until the client exits
  (close-input-port in)
  (close-output-port out))

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
  (string-append (bytes->string/latin-1 #"\033\133")
                 (color-value color) "m"
                 text
                 (bytes->string/latin-1 #"\033\1330m")))

(define (print-banner out)
  (display (colorize 'yellow "=== ") out)
  (display (colorize 'cyan "WELCOME") out)
  (displayln (colorize 'yellow " ===") out))

(define (print-prompt out)
  (display (colorize 'red ">") out)
  (display " " out))

(define (next-line in out)
  (print-prompt out)

  (define line (read-line in))
  
  (cond
    ; stop if there is an EOF. The client has disconnected.
    [(eof-object? line) #f]
    ; always exit when it is typed.
    ; this would be better off implemented as command so that it doesn't apply
    ; to non-command input
    [(equal? (string-trim line) "exit")
     (displayln "goodbye" out)]
    ; skip empty lines
    [(empty-trimmed-string? line) (next-line in out)]
    ; unless a handler is in flight, a new command was typed
    [else
     (if current-handler
         (current-handler line)
         (next-command line out)
         )
     (next-line in out)]))

(define current-handler #f)

(define (next-command line out)
  (define words (string-split line))
  (define command (first words))
  (define (print text) (displayln text out))
  (define session (session-ctx command
                               (rest words)
                               print))
  (define handler (get-handler command))
  
  (if handler
      (handler session)
      (begin
        (print "I don't understand")
        #f)))
