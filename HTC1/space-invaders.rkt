(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 25)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define TANK-ELEVATION 10)



;; Data Definitions:

(define-struct game (invaders missiles t))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-t s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

(define I4 (make-invader (/ WIDTH 3) (/ HEIGHT 3) 0))   ; invader at 1/3rd width and i/3rd Height
(define I5 (make-invader 0 HEIGHT 0))                   ; invader at bottom left corner
(define I6 (make-invader WIDTH 0 0))                    ; invader at top right corner



#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
(define M4 (make-missile (/ WIDTH 2) (/ HEIGHT 2)))      ; in the middle of the screen
(define M5 (make-missile WIDTH HEIGHT))                  ; in the bottom right corner of the screen
(define M6 (make-missile 0 0))                           ; in the top left corner of the screen

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ListOfInvader is one of :
;;   - empty
;;   - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (cons I1 (cons I2 empty)))

#;
(define (fn-for-loinvader loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loinvader (rest loi)))]))

;; Template rules used:
;;    - one of : 2 cases
;;    - Atomic distinct : empty
;;    - Compound : (cons Invader ListOfInvader)
;;    - Reference : (first loi) is Invader
;;    - Self reference : (rest loi) is ListOfInvader




;; ListOfMissiles if one of :
;;   - empty
;;   - (cons Missile ListOfMissile)
;; interp. a list of missiles


(define LOM1 empty)
(define LOM2 (cons M1 (cons M2 empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))


;; Template rules used :
;;   - one of : 2 cases
;;   - Atomic Distince : empty
;;   - Compound : (cons Missile ListOfMissile)
;;   - Reference : (first lom) is Missile
;;   - Self-reference : (rest lom) is ListOfMissile




;; ===================
;; Functions :



;; Game -> Game
;; the main function. To start (main G0).
;;(main (make-game (cons (make-invader 70 90 -5) (cons (make-invader 80 63 5) (cons (make-invader 34 89 -6) empty))) (cons (make-missile 70 HEIGHT) (cons (make-missile 60 200) empty)) (make-tank 50 -1)))
;; no tests for main function

(define (main g)
  (big-bang g
            (on-tick    update-game)     ; Game -> Game
            (to-draw    render-game)     ; Game -> Image
            (stop-when  game-over? )      ; Game -> Boolean
            (on-key     handle-keys)))   ; Game KeyEvent -> Game



;; Game -> Image
;; to produce an image that is equivalent to the position and number of the invaders, missiles and tank. Randomly adds invaders to the screen.
(check-expect (render-game (make-game empty empty T0))
              (place-image TANK (tank-x T0) (- HEIGHT TANK-ELEVATION) BACKGROUND))
(define IMG-AFTER-TANK (place-image TANK
                                    (tank-x T1)
                                    (- HEIGHT TANK-ELEVATION)
                                    BACKGROUND))
(define IMG-AFTER-MISSILES (place-image MISSILE
                                        (missile-x M4)
                                        (missile-y M4)
                                        (place-image MISSILE
                                                     (missile-x M5)
                                                     (missile-y M5)
                                                     (place-image MISSILE
                                                                  (missile-x M6)
                                                                  (missile-y M6)
                                                                  IMG-AFTER-TANK))))
(define IMG-AFTER-INVADERS (place-image INVADER
                                        (invader-x I4)
                                        (invader-y I4)
                                        (place-image INVADER
                                                     (invader-x I5)
                                                     (invader-y I5)
                                                     (place-image INVADER
                                                                  (invader-x I6)
                                                                  (invader-y I6)
                                                                  IMG-AFTER-MISSILES))))

(check-expect (render-game (make-game (cons I4 (cons I5 (cons I6 empty))) (cons M4 (cons M5 (cons M6 empty))) T1)) IMG-AFTER-INVADERS)

;(define (render-game g) BACKGROUND)       ; Stub

;; The function is a composite function

(define (render-game g)
  (render-invaders (game-invaders g) (render-missiles (game-missiles g) (render-tank (game-t g) BACKGROUND))))



;; Tank Image -> Image
;; places the tank at appropriate position in the given image and hence produces a new image
(check-expect (render-tank T0 BACKGROUND)
              (place-image TANK (tank-x T0) (- HEIGHT TANK-ELEVATION) BACKGROUND))
(check-expect (render-tank T1 BACKGROUND) IMG-AFTER-TANK)
              
;(define (render-tank t img) img)           ; Stub

;; template borrowed from tank

(define (render-tank t img)
  (place-image TANK (tank-x t) (- HEIGHT TANK-ELEVATION) img))



;; ListOfMissile Image -> Image
;; produces a new image by placing all the missiles in the given list of missiles into the given image
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (cons (make-missile (/ WIDTH 4) (/ HEIGHT 4)) empty) BACKGROUND)
              (place-image MISSILE (/ WIDTH 4) (/ HEIGHT 4) BACKGROUND))
(check-expect (render-missiles (cons M4 (cons M5 (cons M6 empty))) IMG-AFTER-TANK) IMG-AFTER-MISSILES)

;(define (render-missiles lom img) img)     ; Stub

;; template borrowed from ListOfMissile

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (render-missile (first lom) (render-missiles (rest lom) img))]))



;; Missile Image -> Image
;; produces a new image by placing the missile in the given image
(check-expect (render-missile (make-missile (/ WIDTH 4) (/ HEIGHT 4)) BACKGROUND)
              (place-image MISSILE (/ WIDTH 4) (/ HEIGHT 4) BACKGROUND))
              
;(define (render-missile m img) img)         ; Stub

;; template borrowed from Missile

(define (render-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))



;; ListOfInvader Image -> Image
;; places all the invaders in the list of invaders in the given image and hence produces a new image
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (cons (make-invader (/ WIDTH 4) (/ HEIGHT 4) 0) empty) BACKGROUND)
              (place-image INVADER (/ WIDTH 4) (/ HEIGHT 4) BACKGROUND))
(check-expect (render-invaders (cons I4 (cons I5 (cons I6 empty))) IMG-AFTER-MISSILES) IMG-AFTER-INVADERS)

;(define (render-invaders loi img) img)     ; Stub

;; template borrowed from ListOfInvader

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (render-invader (first loi) (render-invaders (rest loi) img))]))



;; Invader Image -> Image
;; to produce an image by placing Invader in the given image
(check-expect (render-invader (make-invader (/ WIDTH 4) (/ HEIGHT 4) 0) BACKGROUND)
              (place-image INVADER (/ WIDTH 4) (/ HEIGHT 4) BACKGROUND))
              
;(define (render-invader i img) img)        ; Stub

;; template borrowed from Invader

(define (render-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))

;; ==============================================================


;; Game -> Game
;; to produce the next state of the game. i.e. update the invaders' list and position, missiles' list and position and the position of the tank.
;; removes invaders from the list of invaders if invader is hit. (i.e position of invader is withing HIT-RANGE of a missile)
;; removes the missiles that are out of the screen from the list
;; adds invaders randomly from the top of the screen (i.e the new invaders will always have initial y axis of 0)

;; checks for updating positions
(check-expect (update-game (make-game empty empty (make-tank 20 1)))    ; update only tank
              (make-game empty empty (make-tank (+ 20 TANK-SPEED) 1)))  ; tank direction 1

(check-expect (update-game (make-game empty
                                      (cons (make-missile 30 40) (cons (make-missile 150 70) empty))
                                      (make-tank 50 -1)))    ; update missiles and tank position
              (make-game empty (cons (make-missile 30 (- 40 MISSILE-SPEED)) (cons (make-missile 150 (- 70 MISSILE-SPEED)) empty)) (make-tank (- 50 TANK-SPEED) -1))) ; tank direction - 1

(check-expect (update-game (make-game (cons (make-invader 60 70 -15) (cons (make-invader 170 120 10) empty))
                                      empty
                                      (make-tank WIDTH 1))) ; update invaders and tank position 
              (make-game (cons (make-invader (+ 60 (- 15)) (+ 70 INVADER-Y-SPEED) -15) (cons (make-invader (+ 170 10) (+ 120 INVADER-Y-SPEED) 10) empty))
                         empty
                         (make-tank WIDTH 1))) ; tank direction at limit

;; change direction when invaders hit wall

(check-expect (update-game (make-game (cons (make-invader (- WIDTH 2) 70 15) (cons (make-invader 2 60 -10) empty))
                                      empty
                                      (make-tank 0 -1)))
              (make-game (cons (make-invader WIDTH (+ 70 INVADER-Y-SPEED) -15) (cons (make-invader 0 (+ 60 INVADER-Y-SPEED) 10) empty))
                         empty
                         (make-tank 0 -1)))

;; filter missiles
(check-expect (update-game (make-game empty
                                      (cons (make-missile (+ WIDTH 50) 40) (cons (make-missile 150 (+ HEIGHT 30)) empty))
                                      (make-tank 50 -1)))
              (make-game empty empty (make-tank (- 50 TANK-SPEED) -1)))

;(define (update-game g) g)                ; Stub

;; this is a composite function

(define (update-game g)
  (invade-randomly (filter-hits (update-position (filter-missiles g)))))




;; Game -> Game
;; updates the position of all the elements of the game (invaders, missiles and tank) and produces a new game. Changes the direction of invaders if the invaders hit the wall
(check-expect (update-position (make-game empty empty (make-tank 20 1)))
              (make-game empty empty (make-tank (+ 20 TANK-SPEED) 1)))

(check-expect (update-position (make-game empty                  ; update missiles and tank position  
                                          (cons (make-missile 30 40) (cons (make-missile 150 70) empty))
                                          (make-tank 50 -1)))   
              (make-game empty (cons (make-missile 30 (- 40 MISSILE-SPEED)) (cons (make-missile 150 (- 70 MISSILE-SPEED)) empty)) (make-tank (- 50 TANK-SPEED) -1))) ; tank direction - 1

(check-expect (update-position (make-game (cons (make-invader 60 70 -15) (cons (make-invader 170 120 10) empty))   ; update invaders and tank position 
                                          empty
                                          (make-tank WIDTH 1))) 
              (make-game (cons (make-invader (+ 60 (- 15)) (+ 70 INVADER-Y-SPEED) -15) (cons (make-invader (+ 170 10) (+ 120 INVADER-Y-SPEED) 10) empty))
                         empty
                         (make-tank WIDTH 1))) ; tank direction at limit

;; change direction when invaders hit wall
(check-expect (update-position (make-game (cons (make-invader (- WIDTH 2) 70 15) (cons (make-invader 2 60 -10) empty))
                                          empty
                                          (make-tank 0 -1)))
              (make-game (cons (make-invader WIDTH (+ 70 INVADER-Y-SPEED) -15) (cons (make-invader 0 (+ 60 INVADER-Y-SPEED) 10) empty))
                         empty
                         (make-tank 0 -1)))
              
;(define (update-position g) g)             ; Stub

;; template borrowed from Game

(define (update-position s)
  (make-game (update-invader-list (game-invaders s))
             (update-missile-list (game-missiles s))
             (update-tank (game-t s))))



;; ListOfInvader -> ListOfInvader
;; produces an updated list of invaders by updating postion of each invader and changing their dx value if needed (i.e when the invader bounces off a wall)
(check-expect (update-invader-list empty) empty)
(check-expect (update-invader-list (cons (make-invader 60 70 -15) (cons (make-invader 170 120 10) empty)))
              (cons (make-invader (+ 60 (- 15)) (+ 70 INVADER-Y-SPEED) -15) (cons (make-invader (+ 170 10) (+ 120 INVADER-Y-SPEED) 10) empty)))

(check-expect (update-invader-list (cons (make-invader (- WIDTH 3) (/ HEIGHT 2) 9) (cons (make-invader 1 (/ HEIGHT 2) -10) empty)))
              (cons (make-invader WIDTH (+ (/ HEIGHT 2) INVADER-Y-SPEED) (- 9)) (cons (make-invader 0 (+ (/ HEIGHT 2) INVADER-Y-SPEED) (- -10)) empty)))

;(define (update-invader-list loi) loi)      ; Stub

;; template borrowed from ListOfInvador

(define (update-invader-list loi)
  (cond [(empty? loi) empty]
        [else
         (cons (update-each-invader (first loi))
               (update-invader-list (rest loi)))]))



;; Invader -> Invader
;; produces a new Invader by updating the postion and velocity of the current invader
(check-expect (update-each-invader (make-invader 80 100 -5)) (make-invader (+ 80 -5) (+ 100 INVADER-Y-SPEED) -5))
(check-expect (update-each-invader (make-invader 80 100 5)) (make-invader (+ 80 5) (+ 100 INVADER-Y-SPEED) 5))
(check-expect (update-each-invader (make-invader (- WIDTH 5) (/ HEIGHT 4) 10)) (make-invader WIDTH (+ (/ HEIGHT 4) INVADER-Y-SPEED) -10)) ; Changes direction after touching wall
(check-expect (update-each-invader (make-invader 3 (- HEIGHT 50) -10)) (make-invader 0 (+ (- HEIGHT 50) INVADER-Y-SPEED) 10))  ; Changes direction after touching wall

;(define (update-each-invader i) i)           ; Stub

;; template borrowed from Invader

(define (update-each-invader i)
  (cond [(>= (+ (invader-x i) (invader-dx i)) WIDTH)
         (make-invader WIDTH (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
        [(<= (+ (invader-x i) (invader-dx i)) 0)
         (make-invader 0 (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))]))




;; ListOfMissile -> ListOfMissile
;; produces an updated list of missile by updating the y postion of each missile by MISSILE-SPEED
(check-expect (update-missile-list empty) empty)
(check-expect (update-missile-list (cons (make-missile 30 40) (cons (make-missile 150 70) empty)))
              (cons (make-missile 30 (- 40 MISSILE-SPEED)) (cons (make-missile 150 (- 70 MISSILE-SPEED)) empty)))
              
;(define (update-missile-list lom) lom)      ; Stub

;; template borrowed from ListOfMissile

(define (update-missile-list lom)
  (cond [(empty? lom) empty]
        [else
         (cons (update-each-missile (first lom))
               (update-missile-list (rest lom)))]))



;; Missile -> Missile
;; produces a new missile by updating the y position of the missile by MISSILE-SPEED
(check-expect (update-each-missile (make-missile 40 50)) (make-missile 40 (- 50 MISSILE-SPEED)))

;(define (update-each-missile m) m)            ; Stub

;; template borrowed from Missile

(define (update-each-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))



;; Tank -> Tank
;; produces the next position of tank depending of dir. (increments / decrements by TANK-SPEED
(check-expect (update-tank (make-tank 90 -1)) (make-tank (- 90 TANK-SPEED) -1))
(check-expect (update-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))
(check-expect (update-tank (make-tank 0 1)) (make-tank (+ 0 TANK-SPEED) 1))
(check-expect (update-tank (make-tank 0 -1)) (make-tank 0 -1))
(check-expect (update-tank (make-tank 50 -1)) (make-tank (- 50 TANK-SPEED) -1))

;(define (update-tank t) t)                  ; Stub

;; template borrowed from Tank

(define (update-tank t)
  (if (negative? (tank-dir t))
      (if (< (- (tank-x t) TANK-SPEED) 0)
          (make-tank 0 (tank-dir t))
          (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t)))
      (if (> (+ (tank-x t) TANK-SPEED) WIDTH)
          (make-tank WIDTH (tank-dir t))
          (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t)))))



;; Game -> Game
;; filters out the missiles (that are not visible from the screen) in the missile list of the game and produces a new game without altering other elements of game
(check-expect (filter-missiles (make-game empty
                                          (cons (make-missile (+ WIDTH 50) 40) (cons (make-missile 150 (+ HEIGHT 30)) empty))
                                          (make-tank 50 -1)))
              (make-game empty empty (make-tank 50 -1)))



;(define (filter-missiles g) g)             ; Stub

;; template borrowed from Game

(define (filter-missiles s)
  (make-game (game-invaders s)
             (filter-missile-list (game-missiles s))
             (game-t s)))

;; ListOfMissile -> ListOfMissile
;; removes those missile from list which are not visible

(check-expect (filter-missile-list empty) empty)
(check-expect (filter-missile-list (cons (make-missile WIDTH 0) (cons (make-missile (+ WIDTH 50) 60) (cons (make-missile 80 (- 0 50)) empty))))
              (cons (make-missile WIDTH 0) empty))
(check-expect (filter-missile-list (cons (make-missile (+ WIDTH 90) -30) (cons (make-missile 80 130) (cons (make-missile -70 HEIGHT) empty))))
              (cons (make-missile 80 130) empty))

;(define (filter-missile-list lom) lom)       ; Stub

;; template borrowed from ListOfMissile

(define (filter-missile-list lom)
  (cond [(empty? lom) empty]
        [else
         (if (missile-visible? (first lom))
             (cons (first lom) (filter-missile-list(rest lom)))
             (filter-missile-list (rest lom)))]))



;; Missile -> Boolean
;; produces true if the missile is visible from the screen. (i.e the x and y position of missile is within WIDTH and HEIGHT)
(check-expect (missile-visible? (make-missile 80 70)) true)
(check-expect (missile-visible? (make-missile (+ WIDTH 5) -15)) false)
(check-expect (missile-visible? (make-missile 90 (+ HEIGHT 30))) false)
(check-expect (missile-visible? (make-missile WIDTH HEIGHT))      true)
(check-expect (missile-visible? (make-missile 90 0))              true)

;(define (missile-visible? m) false)          ; Stub

;; template borrowed from Missile

(define (missile-visible? m)
  (and (and (>= (missile-x m) 0)
            (<= (missile-x m) WIDTH))
       (and (>= (missile-y m) 0)
            (<= (missile-y m) HEIGHT))))



;; Game -> Game
;; removes the invaders that have been hit (i.e BOTH x and y position of the invader is within the HIT-RANGE of the missile) from the invader list and produces a new game
;; also removes the missile
(check-expect (filter-hits (make-game empty empty T0)) (make-game empty empty T0))
(check-expect (filter-hits (make-game (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) empty))
                                      empty
                                      (make-tank 30 1)))
              (make-game (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) empty))
                         empty
                         (make-tank 30 1)))

(check-expect (filter-hits (make-game (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) empty))
                                      (cons (make-missile 50 60) (cons (make-missile (+ 180 (/ HIT-RANGE 2)) (- 190 (/ HIT-RANGE 3))) empty))
                                      (make-tank 70 -1)))
              (make-game empty
                         empty
                         (make-tank 70 -1)))

(check-expect (filter-hits (make-game (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) empty)))  ;; if one hits the other must miss
                                      (cons (make-missile (+ 180 (/ HIT-RANGE 2)) (- 190 (/ HIT-RANGE 3))) (cons (make-missile (- 180 HIT-RANGE) (+ 190 HIT-RANGE)) empty)) 
                                      (make-tank 70 -1)))
              (make-game (cons (make-invader 50 60 -10) (cons (make-invader 60 130 9) empty))            ; inner missile must hit first
                         (cons (make-missile (- 180 HIT-RANGE) (+ 190 HIT-RANGE)) empty)
                         (make-tank 70 -1)))

(check-expect (filter-hits (make-game (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) empty)))  ;; if one hits the other must miss
                                      (cons (make-missile (+ 60 (/ HIT-RANGE 2)) (- 130 (/ HIT-RANGE 3)))
                                            (cons (make-missile (- 180 HIT-RANGE) (+ 190 HIT-RANGE))
                                                  (cons (make-missile WIDTH HEIGHT)
                                                        (cons (make-missile 60 130) empty)))) 
                                      (make-tank 70 -1)))
              (make-game (cons (make-invader 50 60 -10) empty)            ; inner missile must hit first
                         (cons (make-missile WIDTH HEIGHT) (cons (make-missile 60 130) empty))
                         (make-tank 70 -1)))
              

;(define (filter-hits g) g)              ; Stub

;; template borrowed from Game

(define (filter-hits s)
  (make-game (filter-for-invaders (game-invaders s) (game-missiles s))
             (filter-for-missiles (game-invaders s) (game-missiles s))
             (game-t s)))



;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; produces a new list of Invaders after all the invaders that were hit (i.e. both x and y position difference was less than or equal to HIT-RANGE) were removed
;; One missile can remove only one invader
(check-expect (filter-for-invaders empty empty) empty)
(check-expect (filter-for-invaders (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) empty)) empty) (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) empty)))
(check-expect (filter-for-invaders empty (cons (make-missile (+ 180 (/ HIT-RANGE 2)) (- 190 (/ HIT-RANGE 3))) (cons (make-missile (- 180 HIT-RANGE) (+ 190 HIT-RANGE)) empty))) empty)
(check-expect (filter-for-invaders (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) empty))
                                   (cons (make-missile 50 60) (cons (make-missile (+ 180 (/ HIT-RANGE 2)) (- 190 (/ HIT-RANGE 3))) empty)))
              empty)
(check-expect (filter-for-invaders (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) empty)))
                                   (cons (make-missile (+ 180 (/ HIT-RANGE 2)) (- 190 (/ HIT-RANGE 3))) (cons (make-missile (- 180 HIT-RANGE) (+ 190 HIT-RANGE)) empty)))
              (cons (make-invader 50 60 -10) (cons (make-invader 60 130 9) empty)))
(check-expect (filter-for-invaders (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) empty)))
                                   (cons (make-missile (+ 60 (/ HIT-RANGE 2)) (- 130 (/ HIT-RANGE 3)))
                                         (cons (make-missile (- 180 HIT-RANGE) (+ 190 HIT-RANGE))
                                               (cons (make-missile WIDTH HEIGHT)
                                                     (cons (make-missile 60 130) empty)))))
              (cons (make-invader 50 60 -10) empty))
              
              

;(define (filter-for-invaders loi lom) loi)  ; Stub


;; template borrowed from ListOfInvader

(define (filter-for-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (filter-for-invaders (filter-for-each-missile (first lom) loi) (rest lom))]))



;; Missile ListOfInvader -> ListOfInvader
;; produces a new list of missile where none of the invaders are hit by the given missile. i,e if hit, it removes the missile from the list and returns a new list
(check-expect (filter-for-each-missile (make-missile 50 60)
                                       (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) empty))))
              (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) empty)))
(check-expect (filter-for-each-missile (make-missile 60 130)
                                       (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) empty))))
              (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) empty)))
(check-expect (filter-for-each-missile (make-missile 180 190)
                                       (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) empty))))
              (cons (make-invader 50 60 -10) (cons (make-invader 60 130 9) empty)))
(check-expect (filter-for-each-missile (make-missile 0 0)
                                       (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) empty))))
              (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) empty))))
(check-expect (filter-for-each-missile (make-missile 50 60)
                                       (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) (cons (make-invader 50 60 -10) empty)))))
              (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) (cons (make-invader 50 60 -10) empty))))

;(define (filter-for-each-missile m loi) loi)   ; Stub

;; template borrowed from ListOfInvader

(define (filter-for-each-missile m loi)
  (cond [(empty? loi) empty]
        [else
         (if (hits? m (first loi))
             (rest loi)
             (cons (first loi) (filter-for-each-missile m (rest loi))))]))

;; Missile Invader -> Boolean
;; produces true if the missile hits the invader (i.e thier x and y coordinate differences are BOTH less than HIT-RANGE)
(check-expect (hits? (make-missile 70 70) (make-invader (+ 70 HIT-RANGE 1) (+ 70 HIT-RANGE 1) -10)) false) ; both co-ordinate out of range
(check-expect (hits? (make-missile 50 50) (make-invader (- 50 (/ HIT-RANGE 2)) (+ 50 (/ HIT-RANGE 2)) 20)) true) ; middle case
(check-expect (hits? (make-missile 150 150) (make-invader 150 (+ 50 (/ HIT-RANGE 2)) 20)) false)  ; y coordinate out of range
(check-expect (hits? (make-missile 150 150) (make-invader 40 150 20)) false)  ; x coordinate out of range
(check-expect (hits? (make-missile 150 150) (make-invader (+ 150 HIT-RANGE) (- 150 HIT-RANGE) 20)) true)  ; edge case

;(define (hits? m i) false)                       ; Stub

;; template borrowed from Missile

(define (hits? m i)
  (and (<= (abs (- (missile-x m) (invader-x i))) HIT-RANGE)
       (<= (abs (- (missile-y m) (invader-y i))) HIT-RANGE)))
  


;; ListOfInvaders ListOfMissiles -> ListOfMissiles
;; produces a new list of Missiles after all the missiles that hit an invader were removed.
;; (one missile hits only one invader)
(check-expect (filter-for-missiles empty empty) empty)
(check-expect (filter-for-missiles (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) empty)) empty) empty)
(check-expect (filter-for-missiles empty (cons (make-missile (+ 180 (/ HIT-RANGE 2)) (- 190 (/ HIT-RANGE 3))) (cons (make-missile (- 180 HIT-RANGE) (+ 190 HIT-RANGE)) empty)))
              (cons (make-missile (+ 180 (/ HIT-RANGE 2)) (- 190 (/ HIT-RANGE 3))) (cons (make-missile (- 180 HIT-RANGE) (+ 190 HIT-RANGE)) empty)))
(check-expect (filter-for-missiles (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) empty))
                                   (cons (make-missile 50 60) (cons (make-missile (+ 180 (/ HIT-RANGE 2)) (- 190 (/ HIT-RANGE 3))) empty)))
              empty)
(check-expect (filter-for-missiles (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) empty)))
                                   (cons (make-missile (+ 180 (/ HIT-RANGE 2)) (- 190 (/ HIT-RANGE 3))) (cons (make-missile (- 180 HIT-RANGE) (+ 190 HIT-RANGE)) empty)))
              (cons (make-missile (- 180 HIT-RANGE) (+ 190 HIT-RANGE)) empty))
(check-expect (filter-for-missiles (cons (make-invader 50 60 -10) (cons (make-invader 180 190 7) (cons (make-invader 60 130 9) empty)))
                                   (cons (make-missile (+ 60 (/ HIT-RANGE 2)) (- 130 (/ HIT-RANGE 3)))
                                         (cons (make-missile (- 180 HIT-RANGE) (+ 190 HIT-RANGE))
                                               (cons (make-missile WIDTH HEIGHT)
                                                     (cons (make-missile 60 130) empty)))))
              (cons (make-missile WIDTH HEIGHT) (cons (make-missile 60 130) empty)))

;(define (filter-for-missiles loi lom) lom)  ; Stub

;; template borrowed from ListOfInvaders

(define (filter-for-missiles loi lom)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [else
         (filter-for-missiles (rest loi) (filter-for-each-invader (first loi) lom))]))



;; Invader ListOfMissile -> ListOfMissile
;; produdes a new list of missiles where all the missiles that hit the invaders have been removed. (i,e if any of the missile in the list hits the invader, the invader is remo
(check-expect (filter-for-each-invader (make-invader 50 60 9) empty) empty)
(check-expect (filter-for-each-invader (make-invader 90 120 2) (cons (make-missile 40 50) (cons (make-missile 90 120) empty))) ; one hit
              (cons (make-missile 40 50) empty))
(check-expect (filter-for-each-invader (make-invader 70 60 5) (cons (make-missile 40 50) (cons (make-missile 90 120) empty))) ;  none hit
              (cons (make-missile 40 50) (cons (make-missile 90 120) empty)))

(check-expect (filter-for-each-invader (make-invader 180 190 7) (cons (make-missile (+ 60 (/ HIT-RANGE 2)) (- 130 (/ HIT-RANGE 3))) ; middle hit
                                                                      (cons (make-missile (- 180 HIT-RANGE) (+ 190 HIT-RANGE))
                                                                            (cons (make-missile WIDTH HEIGHT)
                                                                                  (cons (make-missile 60 130) empty)))))
              (cons (make-missile (+ 60 (/ HIT-RANGE 2)) (- 130 (/ HIT-RANGE 3)))
                    (cons (make-missile WIDTH HEIGHT)
                          (cons (make-missile 60 130) empty))))

(check-expect (filter-for-each-invader (make-invader 60 80 9)
                                       (cons (make-missile 50 60) (cons (make-missile 60 80) (cons (make-invader 60 130 -2) (cons (make-invader 60 80 -10) empty)))))
              (cons (make-missile 50 60) (cons (make-invader 60 130 -2) (cons (make-invader 60 80 -10) empty))))
              

;(define (filter-for-each-invader i lom) lom)   ; Stub

;; template borrowed from ListOfMissile

(define (filter-for-each-invader i lom)
  (cond [(empty? lom) empty]
        [else
         (if (hits? (first lom) i)
             (rest lom)
             (cons (first lom) (filter-for-each-invader i (rest lom))))]))



;; Game -> Game
;; randomly inserts invaders into the top of the screen and produces a new game with added invaders.
;; <no tests to check for randominzing functions>

;(define (invade-randomly g) g)              ; Stub

;; template borrowed from Game

(define (invade-randomly s)
  (make-game (add-invaders-randomly (game-invaders s))
             (game-missiles s)
             (game-t s)))

;; ListOfInvader -> ListOfInvader
;; consumes a list of invader and randomly adds an invader to the list of invader
;; <no tests for functions that act randomly>

;(define (add-invaders-randomly loi) loi)     ; Stub

;; template borrowed from ListOfInvader

(define (add-invaders-randomly loi)
  (if (= (random INVADE-RATE) (random INVADE-RATE))
      (cons (make-invader (random WIDTH) 0 (random-dir INVADER-X-SPEED)) loi)
      loi))



;; Integer -> Integer
;; randomly return positive or negative value of the passed integer
;; <no tests for functions that act randomly>

;(define (random-dir n) n)         ; Stub

;; template for random-dir
#;
(define (fn-for-random-dir n)
  (... n))

(define (random-dir n)
  (if (odd? (random 100))
      (- n)
      n))


;; =====================================

;; Game KeyEvent -> Game
;; handles the keys. Moves the tank left/right when left/right key is pressed. adds a missile to the x position of tank (with HEIGHT 0)  when space key is pressed.
(check-expect (handle-keys (make-game (cons (make-invader 40 80 07) empty) (cons (make-missile 70 80) empty) (make-tank 70 -1)) " ")
              (make-game (cons (make-invader 40 80 07) empty) (cons (make-missile 70 (- HEIGHT TANK-ELEVATION)) (cons (make-missile 70 80) empty)) (make-tank 70 -1)))

(check-expect (handle-keys (make-game (cons (make-invader 40 80 07) empty) (cons (make-missile 70 80) empty) (make-tank 70 -1)) "right")
              (make-game (cons (make-invader 40 80 07) empty) (cons (make-missile 70 80) empty) (make-tank 70 1)))

(check-expect (handle-keys (make-game (cons (make-invader 40 80 07) empty) (cons (make-missile 70 80) empty) (make-tank 90 1)) "left")
              (make-game (cons (make-invader 40 80 07) empty) (cons (make-missile 70 80) empty) (make-tank 90 -1)))

;(define (handle-keys g ke) g)             ; Stub

;; template borrowed from handle-key

(define (handle-keys g ke)
  (cond [(key=? ke " ") (add-missile g)]
        [(key=? ke "left") (turn-tank-left g)]
        [(key=? ke "right") (turn-tank-right g)]
        [else g]))


;; Game -> Game
;; Produces a new game by adding a missile to the list of missiles. (The x position of the missile is equal to the x positon of the tank and y position is (height - TANK-ELEVATION)
(check-expect (add-missile (make-game (cons (make-invader 40 80 07) empty) (cons (make-missile 70 80) empty) (make-tank 70 -1)))
              (make-game (cons (make-invader 40 80 07) empty) (cons (make-missile 70 (- HEIGHT TANK-ELEVATION)) (cons (make-missile 70 80) empty)) (make-tank 70 -1)))

;(define (add-missile g) g)                 ; Stub

;; template borrowed from Game

(define (add-missile s)
  (make-game (game-invaders s)
             (cons (make-missile (tank-x (game-t s)) (- HEIGHT TANK-ELEVATION)) (game-missiles s))
             (game-t s)))



;; Game -> Game
;; produces a new game where the direction of the tank is changed to -1
(check-expect (turn-tank-left (make-game (cons (make-invader 40 80 07) empty) (cons (make-missile 70 80) empty) (make-tank 90 1)))
              (make-game (cons (make-invader 40 80 07) empty) (cons (make-missile 70 80) empty) (make-tank 90 -1)))

;(define (turn-tank-left g) g)              ; Stub

;; template borrowed from Game

(define (turn-tank-left s)
  (make-game (game-invaders s)
             (game-missiles s)
             (make-tank (tank-x (game-t s)) -1)))

;; Game -> Game
;; produces a new game where the direction of the tank is changed to 1
(check-expect (turn-tank-right (make-game (cons (make-invader 40 80 07) empty) (cons (make-missile 70 80) empty) (make-tank 70 -1)))
              (make-game (cons (make-invader 40 80 07) empty) (cons (make-missile 70 80) empty) (make-tank 70 1)))

;(define (turn-tank-right g) g)             ; Stub

;; template borrwoed from Game

(define (turn-tank-right s)
  (make-game (game-invaders s)
             (game-missiles s)
             (make-tank (tank-x (game-t s)) 1)))



;; ===========================================

;; Game -> Boolean
;; returns true if any of the invaders in the list of invaders of the game has a y co-ordinate greater than height
(check-expect (game-over? (make-game empty empty T0)) false)
(check-expect (game-over? (make-game (cons (make-invader 50 90 -5) (cons (make-invader 90 (+ HEIGHT 2) 7) empty)) (cons (make-missile 90 20) empty) (make-tank 9 -1))) true)
(check-expect (game-over? (make-game (cons (make-invader 50 90 -5) (cons (make-invader 90 (- HEIGHT 2) 7) empty)) (cons (make-missile 90 20) empty) (make-tank 9 -1))) false)

;(define (game-over? g) false);                ; Stub

;; template borrowed from Game

(define (game-over? s)
  (if  (touches-bottom? (game-invaders s))
       true
       false))

;; ListOfInvader -> Boolean
;; produces true if any of the invader in the given list of invaders has a y co-cordinate greater than HEIGHT
(check-expect (touches-bottom? empty) false)
(check-expect (touches-bottom? (cons (make-invader 50 90 -5) (cons (make-invader 90 (+ HEIGHT 2) 7) empty))) true)
(check-expect (touches-bottom? (cons (make-invader 50 90 -5) (cons (make-invader 90 (- HEIGHT 2) 7) empty))) false)

;(define (touches-bottom? loi) false)           ; Stub

;; template borrowed from ListOfInvader

(define (touches-bottom? loi)
  (cond [(empty? loi) false]
        [else
         (if (> (invader-y (first loi)) HEIGHT)
             true
             (touches-bottom? (rest loi)))]))


