#lang racket

(provide launch-guess-server)

(require 2htdp/image 2htdp/universe "shared.rkt")

#|---------------------------------------------------------------------------|#

(struct interval (small big) #:transparent)

;; Server's initial state
(define SERVER-STATE-0 (interval LOWER UPPER))

#|---------------------------------------------------------------------------|#

(define (single? current-interval)
  (= (interval-small current-interval)
     (interval-big current-interval)))

(define (guess current-interval)
  (quotient (+ (interval-small current-interval)
               (interval-big current-interval)) 2))

(define (smaller current-interval)
  (interval (interval-small current-interval)
            (max (interval-small current-interval)
                 (sub1 (guess current-interval)))))

(define (bigger current-interval)
  (interval (min (interval-big current-interval)
                 (add1 (guess current-interval)))
            (interval-big current-interval)))

#|---------------------------------------------------------------------------|#

(define (connect s-state client)
  (if (false? s-state)
      (make-bundle SERVER-STATE-0
                   (list (make-mail client (guess SERVER-STATE-0))) '())
      (make-bundle s-state
                   empty (list client))))

(define (next-interval s-state msg)
  (cond [(not (string? msg)) s-state]
        [(string=? msg "up") (bigger s-state)]
        [(string=? msg "down") (smaller s-state)]
        [else s-state]))

(define (handle-msg s-state client msg)
  (define n-interval (next-interval s-state msg))
  (make-bundle n-interval (list (make-mail client (guess n-interval))) '()))

#|---------------------------------------------------------------------------|#

(define (launch-guess-server)
  (universe #f
            (state #t)
            (on-new connect)
            (on-msg handle-msg)))
