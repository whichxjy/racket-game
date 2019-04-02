#lang racket

(provide id? 
         id=?
         (struct-out player)
         (struct-out body)
         get-score
         PLAYER-FATTEN-DELTA
         WIDTH HEIGHT CUPCAKE PLAYER-SIZE 
         SCORE GOTO SERIALIZE
         GOTO-LENGTH)

#|-----------------------------------------------------------------|#

(struct player (id body waypoints) #:prefab)
(struct body (size loc) #:prefab #:mutable)

#|-----------------------------------------------------------------|#

(define id? string?)
(define id=? string=?)

(define SCORE 'score)
(define SERIALIZE 'state)
(define GOTO 'goto)
(define GOTO-LENGTH 3)

(define WIDTH 600)
(define HEIGHT 600)
(define CUPCAKE 10)
(define PLAYER-SIZE (* 3 CUPCAKE)) 
(define PLAYER-FATTEN-DELTA 5)

(define (get-score f)
  (/ (- f PLAYER-SIZE) PLAYER-FATTEN-DELTA))
