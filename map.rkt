#lang racket

(require "room.rkt")

(provide starting-room)

(define starting-room
  (new room% (name "musty basement")))

(define (new-passage to-name fro-name)
  (new passage% (to to-name) (fro fro-name)))

(define (add-room from new-name passage)
  (define new-room (new room% (name new-name)))
  (send from connect new-room passage)
  new-room)

(define shed
  (add-room starting-room "abandoned shed" (new-passage "up" "down")))

(define shady-garden
  (add-room shed "shady garden" (new-passage "in" "out")))
