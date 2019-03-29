#lang racket

(require 2htdp/image (except-in 2htdp/universe left right))

#|-----------------------------------------------------------------------|#

;; Constants

;; assumed display dimensions
(define DISPLAY-WIDTH 2560)
(define DISPLAY-HEIGHT 1440)

; initalization constants
(define PLAYER# 2)
(define DICE# 3)
(define BOARD 2)
(define GRID (* BOARD BOARD))
(define INIT-PLAYER 0)
(define INIT-SPARE-DICE 10)

; The depth at which to limit the game tree
(define AI-DEPTH 4)
(define AI 1)

; graphical constants: territories
(define DICE-OFFSET 6)
(define SIDE 75)
(define OFFSET0 (* 2 SIDE))
(define ROTATION 30)
(define HEX 6)
(define (hexagon color)
  (rotate ROTATION (regular-polygon SIDE HEX "solid" color)))
(define X-OFFSET (image-width (hexagon "black")))
(define Y-OFFSET (* (image-height (hexagon "black")) 3/4))

; graphical constants
(define COLORS 
  (list (make-color 255 0 0 100) 
        (make-color 0 255 0 100) 
        (make-color 0 0 255 100)))
(define FOCUS (rotate ROTATION (regular-polygon SIDE 6 "outline" "black")))
(define D1 (bitmap "images/dice1.png"))
(define D2 (bitmap "images/dice2.png"))
(define D3 (bitmap "images/dice3.png"))
(define D4 (bitmap "images/dice4.png"))
(define IMG-LIST (list D1 D2 D3 D4)) 

(define TEXT-SIZE 25)
(define TEXT-COLOR "black")
(define INSTRUCT 
  "← and → to move among territories, <enter> to mark, <d> to unmark, and <p> to pass")
(define AI-TURN "It's the Mighty AI's turn")
(define YOUR-TURN "It's your turn")
(define INFO-X-OFFSET 100)
(define INFO-Y-OFFSET 50)

(define INSTRUCTIONS (text INSTRUCT TEXT-SIZE TEXT-COLOR))
(define WIDTH (+ (image-width INSTRUCTIONS) 50))
(define HEIGHT 600)
(define (PLAIN)
  (define iw (image-width INSTRUCTIONS))
  (define bw (* SIDE 2 BOARD))
  (set! WIDTH  (+ (max iw bw) 50))
  (set! HEIGHT (+ (* SIDE 2 BOARD) 50))
  (empty-scene WIDTH HEIGHT))
(define (ISCENE)
  (define mt (PLAIN))
  (when (or (> (image-width mt) DISPLAY-WIDTH) (> (image-height mt) DISPLAY-HEIGHT))
    (error 'scene
           "it is impossible to draw a ~s x ~s game scene for your screen"
           (image-width mt) (image-height mt)))
  (place-image INSTRUCTIONS (* .5 WIDTH) (* .9 HEIGHT) mt))

#|-----------------------------------------------------------------------|#

(struct dice-world (src board gt) #:transparent)

(define-values (game game? game-board game-player game-moves)
  (let ()
    (struct game (board player delayed-moves) #:transparent)
    (values game
            game? 
            game-board 
            game-player 
            (lambda (g) (force (game-delayed-moves g))))))

(struct move (action gt) #:transparent)

(struct territory (index player dice x y) #:transparent)

#|-----------------------------------------------------------------------|#

(define (game-tree=? gt1 gt2)
  (and (equal? (game-player gt1) (game-player gt2))
       (equal? (game-board gt1) (game-board gt2))
       (= (length (game-moves gt1)) (length (game-moves gt2)))))

#|-----------------------------------------------------------------------|#

;; Create world

;; Get the row that territory is on
(define (get-row posn)
  (quotient posn BOARD))

;; The x coordinate for territory n of a board 
(define (get-x n)
  (+ OFFSET0
     (if (odd? (get-row n)) 0 (/ X-OFFSET 2))
     (* X-OFFSET (modulo n BOARD))))

;; The y coordinate for territory n of a board 
(define (get-y n)
  (+ OFFSET0 (* Y-OFFSET (get-row n))))

(define (dice)
  (add1 (random DICE#)))

(define (territory-build)
  (for/list ([n (in-range GRID)])
    (territory n (modulo n PLAYER#) (dice) (get-x n) (get-y n))))

(define (no-more-moves? game)
  (empty? (game-moves game)))

(define (no-more-moves-in-world? world)
  (define tree (dice-world-gt world))
  (define board (dice-world-board world))
  (define player (game-player tree))
  (or (no-more-moves? tree)
      (for/and ((t board))
        (= (territory-player t) player))))

(define (create-world-of-dice-and-doom)
  (define board (territory-build))
  (define gamet (game-tree board INIT-PLAYER INIT-SPARE-DICE))
  (define new-world (dice-world #f board gamet))
  (if (no-more-moves-in-world? new-world) 
      (create-world-of-dice-and-doom)
      new-world))

#|-----------------------------------------------------------------------|#

;; Get neigbors

;; Return (list x) if (not b) else empty
(define (add b x)
  (if b '() (list x)))

;; Get the neighbors for a territory on an even row
(define (even-row posn top? bottom? right? left?)
  (append (add (or top? right?)    (add1 (- posn BOARD)))
          (add (or bottom? right?) (add1 (+ posn BOARD)))
          (add top?                (- posn BOARD))
          (add bottom?             (+ posn BOARD))
          (add right?              (add1 posn))
          (add left?               (sub1 posn))))

;; Get the neighbors for a territory on an odd row
(define (odd-row posn top? bottom? right? left?)
  (append (add top?               (- posn BOARD))
          (add bottom?            (+ posn BOARD))
          (add (or top? left?)    (sub1 (- posn BOARD)))
          (add (or bottom? left?) (sub1 (+ posn BOARD)))
          (add right?             (add1 posn))
          (add left?              (sub1 posn))))

;; Get the neighbors of the current spot
(define (neighbors posn)  
  (define top?      (< posn BOARD))
  (define bottom?   (= (get-row posn) (sub1 BOARD)))
  (define even-row? (zero? (modulo (get-row posn) 2)))
  (define right?    (zero? (modulo (add1 posn) BOARD)))
  (define left?     (zero? (modulo posn BOARD)))
  (if even-row?
      (even-row posn top? bottom? right? left?)
      (odd-row  posn top? bottom? right? left?)))

#|-----------------------------------------------------------------------|#

;; Game tree

;; Update number of dice on territory (return a new territory)
(define (territory-set-dice trtry dice)
  (territory
   (territory-index trtry)
   (territory-player trtry)
   dice
   (territory-x trtry)
   (territory-y trtry)))

;; Update the owner of the territory (return a new territory)
(define (territory-set-player trtry player)
  (territory
   (territory-index trtry)
   player
   (territory-dice trtry)
   (territory-x trtry)
   (territory-y trtry)))

(define (add-dice-to trtry)
  (territory-set-dice trtry (add1 (territory-dice trtry))))

(define (switch player)
  (modulo (add1 player) PLAYER#))

;; Add reinforcements to the game board
(define (distribute board player spare-dice)
  (for/fold ([dice spare-dice] [new-board '()]) ([t board])
    (if (and (= (territory-player t) player)
             (< (territory-dice t) DICE#)
             (not (zero? dice)))
        (values (- dice 1) (cons (add-dice-to t) new-board))
        (values dice (cons t new-board)))))

;; Check the validity of an attack using the board, the current player,
;; the source territory, and the destination territory
(define (attackable? board player src dst)
  (define dst-t 
    (findf (lambda (t) (= (territory-index t) dst)) board))
  (and dst-t
       (= (territory-player src) player)
       (not (= (territory-player dst-t) player))
       (> (territory-dice src) (territory-dice dst-t))))

;; Create a new board after an attack, update only src and dst
(define (execute board player src dst dice)
  (for/list ([t board])
    (define idx (territory-index t))
    (cond [(= idx src) (territory-set-dice t 1)]
          [(= idx dst) 
           (define s (territory-set-dice t (- dice 1)))
           (territory-set-player s player)]
          [else t])))

;; Make a game tree
(define (game-tree board player dice)
  ;; Create tree of attacks from this position; add passing move
  (define (attacks board) 
    (for*/list ([src board] 
                [dst (neighbors (territory-index src))]
                #:when (attackable? board player src dst))
      (define from (territory-index src))
      (define dice (territory-dice src))
      (define newb (execute board player from dst dice))
      (define attacks-from-newb 
        (game newb player (cons (passes newb) (attacks newb))))
      (move (list from dst) attacks-from-newb)))
  ;; Create a passing move, distribute dice, continue
  (define (passes board)
    (define-values (new-dice newb) (distribute board player dice))
    (move '() (game-tree newb (switch player) new-dice)))
  ;; -- START: -- 
  (game board player (attacks board)))

#|-----------------------------------------------------------------------|#

;; Key-events

(define (left lst)
  (append (rest lst) (list (first lst))))

(define (right lst)
  (reverse (left (reverse lst))))

(define (rotate-until owned-by board rotate)
  (define next-list (rotate board))
  (if (owned-by (territory-player (first next-list))) 
      next-list
      (rotate-until owned-by next-list rotate)))

(define (refocus-board world direction)
  (define source (dice-world-src world))
  (define board  (dice-world-board world))
  (define tree   (dice-world-gt world))
  (define player (game-player tree))
  (define (owner? tid)
    (if source (not (= tid player)) (= tid player)))
  (define new-board (rotate-until owner? board direction))
  (dice-world source new-board tree))

;; find the move from the current list of moves
(define (find-move moves action)
  (define move
    (findf (lambda (m) (equal? (move-action m) action)) moves))
  (and move (move-gt move)))

(define (pass world)
  ;; Find the desired passing move in the list of moves of the game tree
  (define move (find-move (game-moves (dice-world-gt world)) '()))
  (cond [(not move) world]
        [(or (no-more-moves? move) (not (= (game-player move) AI)))
          (dice-world #f (game-board move) move)]
        [else
          (define ai (the-ai-plays move))
          (dice-world #f (game-board ai) ai)]))

(define (attacking world source target)
  (define feasible (game-moves (dice-world-gt world)))
  (define attack   (list source target))
  (define next     (find-move feasible attack))
  (if next
      (dice-world #f (game-board next) next)
      world))

;; Marks a territory as the launching pad for an attack or launches the attack 
(define (mark world)
  (define tree   (dice-world-gt world))
  (define board  (dice-world-board world))
  (define source (dice-world-src world))
  (define focus  (territory-index (first board)))
  (if source 
      (attacking world source focus)
      (dice-world focus board tree)))

;; Unmarks a marked territory
(define (unmark world)
  (dice-world #f (dice-world-board world) (dice-world-gt world)))

(define (interact-with-board world key)
  (cond [(key=? key "left")
         (refocus-board world left)]
        [(key=? key "right")
         (refocus-board world right)]
        [(key=? key "p")
         (pass world)]
        [(key=? key "\r")
         (mark world)]
        [(key=? key "d")
         (unmark world)]
        [else world]))

#|-----------------------------------------------------------------------|#

;; Rendering

(define (color-chooser player-index)
  (list-ref COLORS player-index))

;; Dice

(define (get-dice-image index)
  (list-ref IMG-LIST (modulo index (length IMG-LIST))))

(define (draw-dice num)
  (define first-dice  (get-dice-image 0))
  (define height-dice (image-height first-dice))
  (for/fold ([s first-dice]) ([i (- num 1)])
    (define dice-image (get-dice-image (+ i 1)))
    (define y-offset  (* height-dice (+ .5 (* i .25))))
    (overlay/offset s 0 y-offset dice-image)))

;; Territory

(define (add-territory trtry image scene)
  (place-image image (territory-x trtry) (territory-y trtry) scene))

(define (draw-territory trtry)
  (define color (color-chooser (territory-player trtry)))
  (overlay (hexagon color) (draw-dice (territory-dice trtry))))

;; Player

(define (add-player-info player scene)
  (define str (whose-turn player))
  (define txt (text str TEXT-SIZE TEXT-COLOR))
  (place-image txt (- WIDTH INFO-X-OFFSET) INFO-Y-OFFSET scene))

(define (draw-focus marked? p-in-focus player t-image)
  (if (or (and (not marked?) (= p-in-focus player))
          (and marked? (not (= p-in-focus player))))
      (overlay FOCUS t-image)
      t-image))

;; Board

(define (add-board-to-scene world scene)
  (define board   (dice-world-board world))
  (define player  (game-player (dice-world-gt world)))
  (define focus?  (dice-world-src world))
  (define trtry1  (first board))
  (define p-focus (territory-player trtry1))
  (define t-image (draw-territory trtry1))
  (define image   (draw-focus focus? p-focus player t-image))
  (define base-s  (add-territory trtry1 image scene))
  (for/fold ([s base-s]) ([t (rest board)])
    (add-territory t (draw-territory t) s)))

;; World

(define (draw-dice-world world)
  (add-player-info 
   (game-player (dice-world-gt world)) 
   (add-board-to-scene world (ISCENE))))

(define (draw-end-of-dice-world world)
  (define board (dice-world-board world))
  (define message (text (won board) TEXT-SIZE TEXT-COLOR))
  (define background (add-board-to-scene world (PLAIN)))
  (overlay message background))

#|-----------------------------------------------------------------------|#

;; The end of the game

(define (sum-territory board player)
  (for/fold ([result 0]) ([t board])
    (if (= (territory-player t) player)
        (+ result 1) result)))

(define (winners board)
  (for/fold ([best 0][winners '()]) ([p PLAYER#])
    (define p-score (sum-territory board p))
    (cond [(> p-score best) (values p-score (list p))]
          [(< p-score best) (values best winners)]
          [(= p-score best) (values best (cons p winners))])))

(define (won board)
  (define-values (best-score w) (winners board))
  (if (cons? (rest w)) "It's a tie." "You won."))

#|-----------------------------------------------------------------------|#

;; AI

(define (whose-turn player)
  (if (= player AI) AI-TURN YOUR-TURN))

(define (the-ai-plays tree)
  (define ratings  (rate-moves tree AI-DEPTH))
  (define the-move (first (argmax second ratings)))
  (define new-tree (move-gt the-move))
  (if (= (game-player new-tree) AI)
      (the-ai-plays new-tree)
      new-tree))

(define (rate-moves tree depth)
  (for/list ((move (game-moves tree)))
    (list move (rate-position (move-gt move) (- depth 1)))))

(define (rate-position tree depth)
  (cond [(or (= depth 0) (no-more-moves? tree))
         (define-values (best w) (winners (game-board tree)))
         (if (member AI w) (/ 1 (length w)) 0)]
        [else 
         (define ratings (rate-moves tree depth))
         (apply (if (= (game-player tree) AI) max min)
                (map second ratings))]))

#|-----------------------------------------------------------------------|#

;; Main function
(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)             
            (on-key interact-with-board)
            (to-draw draw-dice-world)
            (stop-when no-more-moves-in-world? 
                       draw-end-of-dice-world)))

;; Start game
(roll-the-dice)
