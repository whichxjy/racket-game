#lang racket

(provide launch-guess-client)

(require 2htdp/image 2htdp/universe "shared.rkt")

#|---------------------------------------------------------------------------|#

;; Client's initial state
(define CLIENT-STATE-0 "no guess available")

#|---------------------------------------------------------------------------|#

;; Help text size and color
(define HELP-TEXT-SIZE 20)
(define HELP-TEXT-COLOR "Light Steel Blue")

(define HELP-TEXT
  (text "↑ for larger numbers, ↓ for smaller ones"
        HELP-TEXT-SIZE
        HELP-TEXT-COLOR))

(define HELP-TEXT2
  (text "Press = when your number is guessed; q to quit."
        HELP-TEXT-SIZE
        HELP-TEXT-COLOR))

;; Number size and color
(define NUM-SIZE 80)
(define NUM-COLOR "Dark Slate Gray")

;; Game window width and height
(define WIDTH (* (image-width HELP-TEXT2) 1.3))
(define HEIGHT 300)

;; Text position
(define TEXT-X 3)
(define TEXT-UPPER-Y (* HEIGHT 0.05))
(define TEXT-LOWER-Y (* HEIGHT 0.95))

;; Background scene
(define BACKGROUND
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
    (place-image/align
     HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
     (empty-scene WIDTH HEIGHT))))

#|---------------------------------------------------------------------------|#

(define (handle-keys c-state key)
  (cond [(key=? key "up") (make-package c-state "up")]
        [(key=? key "down") (make-package c-state "down")]
        [(key=? key "q") (stop-with c-state)]
        [(key=? key "=") (stop-with c-state)]
        [else c-state]))

(define (draw-guess c-state)
  (overlay (text c-state NUM-SIZE NUM-COLOR) BACKGROUND))

(define (handle-msg c-state msg)
  (number->string msg))

#|---------------------------------------------------------------------------|#

(define (launch-guess-client world-name host)
  (big-bang CLIENT-STATE-0
            (on-key handle-keys)
            (on-draw draw-guess)
            (name world-name)
            (register host)
            (on-receive handle-msg)))
