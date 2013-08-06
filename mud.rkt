#lang racket

(require "server.rkt")
(require "prompt.rkt")

(define server
    (serve 1234 new-mud-connection))

(thread-wait (server-control-thread server))
