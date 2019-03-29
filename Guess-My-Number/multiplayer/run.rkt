#lang racket

(require 2htdp/universe "client.rkt" "server.rkt")

(define (run)
  (launch-many-worlds (launch-guess-client "Quantum" LOCALHOST)
                      (launch-guess-server)))

(define (bad)
  (launch-many-worlds (launch-guess-client "Quantum" LOCALHOST) 
                      (launch-guess-server)
                      (launch-guess-client "Dust" LOCALHOST)))
