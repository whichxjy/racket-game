#lang racket

(require 2htdp/universe 2htdp/image)

(struct interval (small big))

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

(define (deal-with-guess current-interval key)
  (cond [(key=? key "up") (bigger current-interval)]
        [(key=? key "down") (smaller current-interval)]
        [(key=? key "q") (stop-with current-interval)]
        [(key=? key "=") (stop-with current-interval)]
        [else current-interval]))

(define (smaller current-interval)
  (interval (interval-small current-interval)
            (max (interval-small current-interval)
                 (sub1 (guess current-interval)))))

(define (bigger current-interval)
  (interval (min (interval-big current-interval)
                 (add1 (guess current-interval)))
            (interval-big current-interval)))

(define (guess current-interval)
  (quotient (+ (interval-small current-interval)
               (interval-big current-interval)) 2))

(define (render current-interval)
  (overlay (text (number->string (guess current-interval))
                 NUM-SIZE NUM-COLOR) BACKGROUND))

(define (render-last-scene current-interval)
  (overlay (text "End" NUM-SIZE NUM-COLOR) BACKGROUND))

(define (single? current-interval)
  (= (interval-small current-interval)
     (interval-big current-interval)))

;; Main function
(define (start lower upper)
  (big-bang (interval lower upper)
   (on-key deal-with-guess)
   (to-draw render)
   (stop-when single? render-last-scene)))

;; Start game
(start 1 100)
