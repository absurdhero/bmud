#lang racket

(require "user.rkt")
(require "handlers.rkt")
(require "map.rkt")
(require "ansi-term.rkt")

(provide new-mud-connection)

; serves clients over an input and output port
(define (new-mud-connection in out)
  (print-banner out)

  (define (close-connection)
    (close-input-port in)
    (close-output-port out))

  (define name (read-username in out))
  (unless name
    (close-connection))
  
  (display "\n" out)

  (unless (user-exists? name)
    (display (colorize 'yellow "I have not seen you before.\n") out)
    (define user (create-user name))
    (user-prop-set! user "room" starting-room))

  (display (colorize 'red "Welcome to the dungeons of BMUD!\n\n") out)

  (next-line (user-get name) in out) ;; processes input until the client exits
  (close-connection))

(define (read-username in out)
  (display "\n" out)
  (display (colorize 'yellow "name: ") out)
  
  (define line (read-line in))

  (if (eof-object? line)
      #f
      (string-trim line)))

(define (empty-trimmed-string? str)
      (zero? (string-length (string-trim str))))

(define (print-banner out)
  (display (colorize 'yellow "=== ") out)
  (display (colorize 'cyan "BMUD") out)
  (displayln (colorize 'yellow " ===") out))

(define (print-prompt out)
  (display (colorize 'red ">") out)
  (display " " out))

(define (next-line user-state in out)
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
    [(empty-trimmed-string? line) (next-line user-state in out)]
    ; unless a handler is in flight, a new command was typed
    [else
     (if current-handler
         (current-handler line)
         (next-command user-state line out)
         )
     (next-line user-state in out)]))

(define current-handler #f)

(define (next-command user-state line out)
  (define words (string-split line))
  (define command (first words))
  (define session (new session-ctx%
                       [command command]
                       [args (rest words)]
                       [user user-state]
                       [out out]))
  (define handler (get-handler command))
  
  (if handler
      (handler session)
      (begin
        (send session print "I don't understand")
        #f)))
