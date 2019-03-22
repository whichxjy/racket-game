#lang racket

(require 2htdp/universe 2htdp/image)

;; Segments size
(define SEG-SIZE 30)

;; Game map size
(define SIZE 20)

;; Tick rate
(define TICK-RATE 0.2)

;; Game windown width and height
(define WIDTH (* SEG-SIZE SIZE))
(define HEIGHT (* SEG-SIZE SIZE))

;; Background scene
(define BACKGROUND (empty-scene WIDTH HEIGHT))

;; Snake head image
(define HEAD-LEFT-IMG (bitmap "images/head.gif"))
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (rotate 180 HEAD-LEFT-IMG))
(define HEAD-UP-IMG (rotate 270 HEAD-LEFT-IMG))

;; Snake body image
(define SEG-IMG (bitmap "images/body.gif"))

;; Goo image
(define GOO-IMG (bitmap "images/goo.gif"))

;; Goo expiration time
(define EXPIRATION-TIME 40)

;; Endgame text size
(define ENDGAME-TEXT-SIZE 30)

#|--------------------------------------------------------|#

(struct pit (snake goos) #:transparent)

(struct snake (dir segs) #:transparent)

(struct goo (loc expire) #:transparent)

(struct posn (x y) #:transparent)

#|--------------------------------------------------------|#

;; Auxiliary functions

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (snake-head snake)
  (first (snake-segs snake)))

(define (snake-body snake)
  (rest (snake-segs snake)))

(define (snake-tail snake)
  (last (snake-segs snake)))

(define (snake-change-dir the-snake dir)
  (snake dir (snake-segs the-snake)))

#|--------------------------------------------------------|#

;; Rotting goo

(define (decay the-goo)
  (goo (goo-loc the-goo) (sub1 (goo-expire the-goo))))

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos))
                    (rot (rest goos)))]))

(define (rotten? goo)
  (zero? (goo-expire goo)))

(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (cons (fresh-goo) (renew (rest goos)))]
        [else (cons (first goos)
                    (renew (rest goos)))]))

(define (age-goo goos)
  (rot (renew goos)))

;; Create new goo
(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))

#|--------------------------------------------------------|#

;; Eating and growing

(define (close? snake-head goo)
  (posn=? snake-head (goo-loc goo)))

(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (grow the-snake)
  (snake (snake-dir the-snake)
         (cons (next-head the-snake) (snake-segs the-snake))))

#|--------------------------------------------------------|#

;; Slithering

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (next-head snake)
  (define head (snake-head snake))
  (define dir (snake-dir snake))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs)
                    (all-but-last (rest segs)))]))

(define (slither the-snake)
  (snake (snake-dir the-snake)
         (cons (next-head the-snake)
               (all-but-last (snake-segs the-snake)))))

#|--------------------------------------------------------|#

;; Rendering

;; Place the image at the position onto the given scene
(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

;; Draw the snake into the given scene
(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(string=? dir "up") HEAD-UP-IMG]
                   [(string=? dir "down") HEAD-DOWN-IMG]
                   [(string=? dir "left") HEAD-LEFT-IMG]
                   [(string=? dir "right") HEAD-RIGHT-IMG])
             snake-body-scene))

;; Draw images at given positions
(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               img
               (img-list+scene (rest posns) img scene))]))

;; Draw the list of goo into the scene
(define (goo-list+scene goos scene)
  (define (get-posns-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos))
                      (get-posns-from-goo (rest goos)))]))
  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))

;; Render the game
(define (render-pit pit)
  (snake+scene (pit-snake pit)
               (goo-list+scene (pit-goos pit) BACKGROUND)))

#|--------------------------------------------------------|#

;; Clock ticks

(define (next-pit pip)
  (define snake (pit-snake pip))
  (define goos (pit-goos pip))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

#|--------------------------------------------------------|#

;; Key-events

(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))

(define (pit-change-dir the-pit dir)
  (define the-snake (pit-snake the-pit))
  (cond [(and (opposite-dir? (snake-dir the-snake) dir)
              ;; consists of the head and at least one seqment
              (pair? (rest (snake-segs the-snake))))
         (stop-with the-pit)]
        [else (pit (snake-change-dir the-snake dir)
                   (pit-goos the-pit))]))

(define (direct-snake pit key)
  (cond [(dir? key) (pit-change-dir pit key)]
        [else pit]))

#|--------------------------------------------------------|#

;; End Game

(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= x 0) (= x SIZE)
      (= y 0) (= y SIZE)))

(define (dead? pit)
  (define snake (pit-snake pit))
  (or (self-colliding? snake) (wall-colliding? snake)))

(define (render-end pit)
  (overlay (text "Game Over" ENDGAME-TEXT-SIZE "black")
           (render-pit pit)))

#|--------------------------------------------------------|#

;; Main function
(define (start-game)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))
    (on-tick next-pit TICK-RATE)
    (on-key direct-snake)
    (to-draw render-pit)
    (stop-when dead? render-end)))

;; Start game
(start-game)
