#lang racket

(provide color-value colorize)

(define (color-value color-name)
  (number->string
   (case color-name
     ['black 30]
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

