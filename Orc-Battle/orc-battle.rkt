#lang racket

(require 2htdp/universe 2htdp/image)

#|----------------------------------------------------------------------|#

#| ========================= Player ========================= |#
;; Player attributes
(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)

(define ATTACKS# 4)
(define STAB-DAMAGE 2)
(define FLAIL-DAMAGE 3)
(define HEALING 8)

#| ========================= Monster ========================= |#
;; The number of monsters to start with
(define MONSTER# 12)
;; How many monsters appear on each row
(define PER-ROW 4)
(unless (zero? (remainder MONSTER# PER-ROW))
  (error 'constraint "PER-ROW must divide MONSTER# evenly into rows"))

(define MONSTER-HEALTH0 9)
(define CLUB-STRENGTH 8)
(define SLIMINESS 5)

(define HEALTH-DAMAGE -2)
(define AGILITY-DAMAGE -3)
(define STRENGTH-DAMAGE -4)

#| ========================= String ========================= |#
(define STRENGTH "strength")
(define AGILITY "agility")
(define HEALTH "health")
(define LOSE  "YOU LOSE")
(define WIN "YOU WIN")
(define DEAD "DEAD")
(define REMAINING "Remaining attacks ")
(define INSTRUCTIONS-2 "Select a monster using the arrow keys")
(define INSTRUCTIONS-1
  "Press S to stab a monster | Press F to Flail wildly | Press H to Heal")

#| ========================= Graphical ========================= |#

(define HEALTH-BAR-HEIGHT 12)
(define HEALTH-BAR-WIDTH  50)

#| ========================= Frames ========================= |#
(define ORC     (bitmap "images/orc.png"))
(define HYDRA   (bitmap "images/hydra.png"))
(define SLIME   (bitmap "images/slime.bmp"))
(define BRIGAND (bitmap "images/brigand.bmp"))

(define PIC-LIST (list ORC HYDRA SLIME BRIGAND))

(define width (apply max (map image-width PIC-LIST)))
(define height (apply max (map image-height PIC-LIST)))

#| ========================= Images ========================= |#
(define PLAYER-IMAGE  (bitmap "images/player.bmp"))

(define FRAME (rectangle width height 'outline 'white))
(define TARGET (circle (- (/ width 2) 2) 'outline 'blue))

(define ORC-IMAGE     (overlay ORC FRAME))
(define HYDRA-IMAGE   (overlay HYDRA FRAME))
(define SLIME-IMAGE   (overlay SLIME FRAME))
(define BRIGAND-IMAGE (overlay BRIGAND FRAME))

(define V-SPACER (rectangle 0 10 "solid" "white"))
(define H-SPACER (rectangle 10 0 "solid" "white"))

#| ========================= Fonts/Texts/Colors ========================= |#
(define AGILITY-COLOR "blue")
(define HEALTH-COLOR "crimson")
(define STRENGTH-COLOR "forest green") 
(define MONSTER-COLOR "crimson")
(define MESSAGE-COLOR "black")
(define ATTACK-COLOR "crimson")

(define HEALTH-SIZE (- HEALTH-BAR-HEIGHT 4))
(define DEAD-TEXT-SIZE (- HEALTH-BAR-HEIGHT 2))
(define INSTRUCTION-TEXT-SIZE 16)
(define MESSAGES-SIZE 40)

(define INSTRUCTION-TEXT
  (above 
   (text INSTRUCTIONS-2 (- INSTRUCTION-TEXT-SIZE 2) "blue")
   (text INSTRUCTIONS-1 (- INSTRUCTION-TEXT-SIZE 4) "blue")))

(define DEAD-TEXT (text DEAD DEAD-TEXT-SIZE "crimson"))

#|----------------------------------------------------------------------|#

#|---------------------------------------------------------------|
 | ========================= Orc World ========================= |
 | [player]: describe the player's attributes                    |
 | [lom]: list of monsters                                       |
 | [attack#]: how many attacks the player may execute            |
 | [target]: which of the monsters in lom is currently targeted  |
 |---------------------------------------------------------------|#
(struct orc-world (player lom attack# target) #:transparent #:mutable)

(struct player (health agility strength) #:transparent #:mutable)

(struct monster (image [health #:mutable]) #:transparent)

;; Four kinds of monsters
(struct orc monster (club) #:transparent)
(struct hydra monster () #:transparent)
(struct slime monster (sliminess) #:transparent)
(struct brigand monster () #:transparent)

#|----------------------------------------------------------------------|#

;; Initializing the orc world

(define (random-quotient x y)
  (define div (quotient x y))
  (if (< div 0) 0 (random+ (add1 div))))

(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH))

(define (random-number-of-attacks player)
  (random-quotient (player-agility player) ATTACKS#))

(define (initialize-monsters)
  (build-list
   MONSTER#
   (lambda (_)
     (define health (random+ MONSTER-HEALTH0))
     (case (random 4)
       [(0) (orc ORC-IMAGE health (random+ CLUB-STRENGTH))]
       [(1) (hydra HYDRA-IMAGE health)]
       [(2) (slime SLIME-IMAGE health (random+ SLIMINESS))]
       [(3) (brigand BRIGAND-IMAGE health)]))))

(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0))

#|----------------------------------------------------------------------|#

(define (random+ num)
  (add1 (random num)))

(define (random- num)
  (- (add1 (random num))))

(define (interval+ n m (max-value 100))
  (interval- n (- m) max-value))

(define (interval- n m (max-value 100))
  (min (max 0 (- n m)) max-value))

#|----------------------------------------------------------------------|#

(define (player-update! setter selector max-value)
  (lambda (player delta)
    (setter player (interval+ (selector player) delta max-value))))

(define player-health+
  (player-update! set-player-health! player-health MAX-HEALTH))

(define player-agility+
  (player-update! set-player-agility! player-agility MAX-AGILITY))

(define player-strength+
  (player-update! set-player-strength! player-strength MAX-STRENGTH))

#|----------------------------------------------------------------------|#

;; Key-events

(define (decrease-attack# world)
  (set-orc-world-attack#! world (sub1 (orc-world-attack# world))))

(define (damage-monster monster delta)
  (set-monster-health! monster
                       (interval- (monster-health monster) delta)))

(define (current-target world)
  (list-ref (orc-world-lom world) (orc-world-target world)))

(define (stab world)
  (decrease-attack# world)
  (define target (current-target world))
  (define damage 
    (random-quotient (player-strength (orc-world-player world))
                     STAB-DAMAGE))
  (damage-monster target damage))

(define (heal world)
  (decrease-attack# world)
  (player-health+ (orc-world-player world) HEALING))

(define (flail world)
  (decrease-attack# world)
  (define target (current-target world))
  (define alive
    (filter monster-alive? (orc-world-lom world)))
  (define pick#
    (min
     (random-quotient (player-strength (orc-world-player world)) 
                      FLAIL-DAMAGE)
     (length alive)))
  (define getem (cons target (take alive pick#)))
  (for-each (lambda (m) (damage-monster m 1)) getem))

(define (end-turn world)
  (set-orc-world-attack#! world 0))

(define (move-target world n) 
  (set-orc-world-target! world
                         (modulo
                          (+ n (orc-world-target world)) MONSTER#)))

(define (all-monsters-attack-player player lom)
  (define (one-monster-attacks-player monster)
    (cond
      [(orc? monster)
       (player-health+ player (random- (orc-club monster)))]
      [(hydra? monster)
       (player-health+ player (random- (monster-health monster)))]
      [(slime? monster) 
       (player-health+ player -1)
       (player-agility+ player (random- (slime-sliminess monster)))]
      [(brigand? monster) 
       (case (random 3)
         [(0) (player-health+ player HEALTH-DAMAGE)]
         [(1) (player-agility+ player AGILITY-DAMAGE)]
         [(2) (player-strength+ player STRENGTH-DAMAGE)])]))
  (for-each one-monster-attacks-player (filter monster-alive? lom)))

(define (give-monster-turn-if-attack#=0 world)
  (when (zero? (orc-world-attack# world))
    (define player (orc-world-player world))
    (all-monsters-attack-player player (orc-world-lom world))
    (set-orc-world-attack#! world (random-number-of-attacks player))))

(define (player-acts-on-monsters world key)
  (cond
    ;;[(zero? (orc-world-attack# world)) (void)]
    [(zero? (orc-world-attack# world)) world]
    [(key=? key "s") (stab world)]
    [(key=? key "h") (heal world)]
    [(key=? key "f") (flail world)]
    [(key=? key "e") (end-turn world)]
    [(key=? key "n") (initialize-orc-world)]
    [(key=? key "right") (move-target world +1)]
    [(key=? key "left") (move-target world -1)]
    [(key=? key "down") (move-target world (+ PER-ROW))]
    [(key=? key "up") (move-target world (- PER-ROW))]
    [else world])
  (give-monster-turn-if-attack#=0 world)
  world)
    

#|----------------------------------------------------------------------|#

;; Rendering

(define (instructions world)
  ;; The number of remaining attacks
  (define na (number->string (orc-world-attack# world)))
  ;; The remaining attacks -> string
  (define ra (string-append REMAINING na))
  ;; Text
  (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
  (above txt INSTRUCTION-TEXT))

(define (message str)
  (text str MESSAGES-SIZE MESSAGE-COLOR))

;; draw player and monsters, then add messages
(define (render-orc-world world target additional-text)
  (define i-player (render-player (orc-world-player world)))
  (define i-monster (render-monsters (orc-world-lom world) target))
  (above V-SPACER
         (beside H-SPACER
                 i-player
                 H-SPACER H-SPACER H-SPACER 
                 (above i-monster
                        V-SPACER V-SPACER V-SPACER 
                        additional-text)
                 H-SPACER)
         V-SPACER))

;; Status bar
(define (status-bar v-current v-max color label)
  ;; Health bar
  (define width
    (* (/ v-current v-max) HEALTH-BAR-WIDTH))
  (define front
    (rectangle width HEALTH-BAR-HEIGHT 'solid color))
  (define back
    (rectangle HEALTH-BAR-WIDTH HEALTH-BAR-HEIGHT 'outline color))
  (define bar
    (overlay/align 'left 'top front back))
  (beside bar H-SPACER (text label HEALTH-SIZE color)))

;; Render player with status bars
(define (render-player player)
  (above/align 
   "left"
   (status-bar (player-strength player) MAX-STRENGTH STRENGTH-COLOR STRENGTH)
   V-SPACER
   (status-bar (player-agility player) MAX-AGILITY AGILITY-COLOR AGILITY)
   V-SPACER
   (status-bar (player-health player) MAX-HEALTH HEALTH-COLOR HEALTH)
   V-SPACER V-SPACER V-SPACER
   PLAYER-IMAGE))

(define (arrange lom)
  (cond
    [(empty? lom) empty-image]
    [else (define row-image (apply beside (take lom PER-ROW)))
          (above row-image (arrange (drop lom PER-ROW)))]))

;; Render monsters with status bars
(define (render-monsters lom with-target)
  ;; Target monster
  (define target
    (if (number? with-target) 
        (list-ref lom with-target)
        'a-silly-symbol-that-cannot-be-eq-to-an-orc))
  ;; Draw a monster
  (define (render-one-monster monster)
    (define image
      (if (eq? monster target)
          (overlay TARGET (monster-image monster))
          (monster-image monster)))
    (define health (monster-health monster))
    (define health-bar
      (if (= health 0)
          (overlay DEAD-TEXT (status-bar 0 1 'white ""))
          (status-bar health MONSTER-HEALTH0 MONSTER-COLOR "")))
    (above health-bar image))
  ;; Break a list of monster images into rows of PER-ROW
  (arrange (map render-one-monster lom)))

;; During the game
(define (render-orc-battle world)
  (render-orc-world world (orc-world-target world) (instructions world)))

;; After the game is over
(define (render-the-end world)
  (render-orc-world world #f (message (if (lose? world) LOSE WIN))))

#|----------------------------------------------------------------------|#

;; End game

(define (monster-alive? monster)
  (> (monster-health monster) 0))

(define (all-dead? lom)
  (not (ormap monster-alive? lom)))

(define (win? world)
  (all-dead? (orc-world-lom world)))

(define (player-dead? player)
  (or (= (player-health player) 0) 
      (= (player-agility player) 0)
      (= (player-strength player) 0)))

(define (lose? world) 
  (player-dead? (orc-world-player world)))

(define (end-of-orc-battle? world)
  (or (win? world) (lose? world)))

#|----------------------------------------------------------------------|#

;; Main function
(define (start-game)
  (big-bang 
    (initialize-orc-world)
    (on-key player-acts-on-monsters)
    (to-draw render-orc-battle)
    (stop-when end-of-orc-battle? render-the-end)))

;; Start game
(start-game)
