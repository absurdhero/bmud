#lang racket

(require "server.rkt")
(require "prompt.rkt")

(define stop
    (serve 1234 new-mud-connection))

