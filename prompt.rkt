#lang racket

(require "handlers.rkt")
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
    (create-user name))

  (display (colorize 'red "Welcome to the dungeons of BMUD!\n\n") out)

  (next-line (user-get name) in out) ;; processes input until the client exits
  (close-connection))

(define users (make-hash))

(define (user-exists? name)
  (hash-has-key? users name))

(define (user-get name)
  (hash-ref users name))

(define (create-user name)
  (define state (make-hash))
  (hash-set! state "name" name)
  (hash-set! state "location" (cons 0 0))
  (hash-set! users name state))

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
  (define (print text) (displayln text out))
  (define session (session-ctx command
                               (rest words)
                               user-state
                               print))
  (define handler (get-handler command))
  
  (if handler
      (handler session)
      (begin
        (print "I don't understand")
        #f)))
