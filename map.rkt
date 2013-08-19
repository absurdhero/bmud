#lang racket

(require "room.rkt")
(require "object-store.rkt")

(provide starting-room mud-root)

; root of all rooms where common commands go

(define mud-root
  (new room% (name "bmud root")))

(define starting-room
  (new room% (name "musty basement")))

(send starting-room set-outer mud-root)

(define (new-passage to-name fro-name)
  (new passage% (to to-name) (fro fro-name)))

(define (add-room from new-name passage)
  (define new-room (new room% (name new-name)))
  (add-object new-room)
  (send new-room set-outer mud-root)
  (send from connect new-room passage)
  new-room)

; map

(define shed
  (add-room starting-room "abandoned shed" (new-passage "up" "down")))

(define shady-garden
  (add-room shed "shady garden" (new-passage "in" "out")))

(define rickety-fence
  (add-room shady-garden "rickety fence" (new-passage "east" "west")))

(define cobblestone-path
  (add-room shady-garden "cobblestone path" (new-passage "west" "east")))
