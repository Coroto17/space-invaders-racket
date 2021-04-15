;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define INVADE-RATE 100)

(define STAR (square 2 "solid" "white"))
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "black"))
(define BACKGROUND-GAME-OVER (place-image (text "GAME\nOVER" 30 "indigo")
                                          (/ WIDTH 2)
                                          (/ HEIGHT 2)
                                          (rectangle WIDTH HEIGHT "solid" "light red")))
(define BACKGROUND-TRANSPARENT (rectangle WIDTH HEIGHT "solid" "transparent"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "light blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "light green")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "dark brown")       ;tread center
                       (ellipse 30 10 "outline" (pen "white" 1 "short-dash" "round" "bevel")))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "brown")       ;gun
                     (rectangle 20 10 "solid" "dark green"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE-HEIGHT 15)
(define MISSILE (ellipse 5 MISSILE-HEIGHT "solid" "yellow"))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (list (make-invader 150 100 12) (make-invader 150 HEIGHT -10)))
(define LOI3 (list (make-invader WIDTH 80 12) (make-invader 130 HEIGHT 10) I3))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Invader ListOfInvader)
;; - reference: (first loi) is Invader
;; - self reference: (rest loi) is ListOfInvader

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. a list of missiles

(define LOM1 empty)
(define LOM2 (list (make-missile 150 100) (make-missile 150 250)))
(define LOM3 (list (make-missile 80 20) (make-missile 130 0) (make-missile 50 (/ HEIGHT 2))))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Missile ListOfMissile)
;; - reference: (first lom) is Missile
;; - self reference: (rest lom) is ListOfMissile

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; Functions:

;; Natural Image-> Image
;; put n stars onto the given image, at random places
(check-expect (put-stars 0 BACKGROUND) BACKGROUND)

;(define (put-stars n img) BACKGROUND)  ; stub

(define (put-stars n img)
  (cond [(zero? n) img]
        [else
         (place-image STAR
                      (random WIDTH)
                      (random (- HEIGHT 10))
                      (put-stars (sub1 n) img))]))

(define BCKG (put-stars 30 BACKGROUND))
(define BCKG-GAME-OVER (put-stars 30 BACKGROUND-GAME-OVER))

;; Game -> Game
;; start the world with (main G0)

(define (main gm)
  (big-bang gm
    (on-tick  next-game)                         ; Game -> Game
    (to-draw  render-game)                       ; Game -> Image
    (on-key handle-keys)                         ; Game KeyEvent -> Game
    (stop-when touch-ground? render-game-over))) ; Game -> Boolean ; Game -> Image

;; Game -> Game
;; produces the next state of the game
(check-expect (next-game (make-game (list I1) empty T1))                                    ; move invader
              (make-game (list (make-invader (+ 150 INVADER-X-SPEED)
                                             (+ 100 INVADER-Y-SPEED)
                                             12))
                         empty 
                         (make-tank (+ (tank-x T1) TANK-SPEED) (tank-dir T1))))
(check-expect (next-game (make-game empty (list (make-missile 20 80)) T1))                    ; move missile
              (make-game empty
                         (list (make-missile 20 (- 80 MISSILE-SPEED)))
                         (make-tank (+ (tank-x T1) TANK-SPEED) (tank-dir T1))))
(check-expect (next-game (make-game empty empty T1))                                          ; move tank righ
              (make-game empty empty
                         (make-tank (+ (tank-x T1) TANK-SPEED) (tank-dir T1))))
(check-expect (next-game (make-game empty empty T2))                                          ; move tank left
              (make-game empty empty
                         (make-tank (- (tank-x T1) TANK-SPEED) (tank-dir T2))))
(check-expect (next-game (make-game (list (make-invader 320 400 -15) (make-invader 80 90 15)) ; move everything
                                    (list (make-missile 340 70) (make-missile 250 250))
                                    T0))                                       
              (make-game (list (make-invader 318.5 401.5 -15) (make-invader 81.5 91.5 15))
                         (list (make-missile 340 60) (make-missile 250 240))
                         (make-tank (+ (/ WIDTH 2) 2) 1)))
(check-expect (next-game (make-game (list (make-invader 50 100 -10))
                                    (list (make-missile 38 120))
                                    T1))
              (make-game (list (make-invader 48.5 101.5 -10))
                         (list (make-missile 38 110))
                         (make-tank 52 1)))

;(define (next-game gm) G0) ; stub

(define (next-game gm)
  (filter-game (advance-game gm)))

;; Game -> Game
;; removes from the game unused parts (destroyed invaders, collididng missiles and gone missiles)
(check-expect (filter-game G0) G0)                                                         ; base case
(check-expect (filter-game (make-game (list I1) (list M2) T1)) (make-game empty empty T1)) ; missile impact invader
(check-expect (filter-game (make-game (list I1) (list (make-missile 50 -10)) T1))          ; missile gone
              (filter-game (make-game (list I1) empty T1)))

;(define (filter-game gm) G0) ; stub

(define (filter-game gm)
  (filter-impact (game-invaders gm)
                 (filter-missiles (game-missiles gm))
                 (game-tank gm)))

;; ListOfInvader ListOfMissile Tank -> Game
;; removes both invader and missile after a collision: y-pos(missile - invader) <= HIT-RANGE
(check-expect (filter-impact empty empty T0) G0)       ; base case
(check-expect (filter-impact (list I1) (list M2) T1)   ; missile impact invader
              (make-game empty empty T1))

;(define (filter-impact loi lom t) G0) ; stub

; <use Template from loi with added lom and Tank>

(define (filter-impact loi lom t)
  (cond [(empty? loi) (make-game empty lom t)]
        [(empty? lom) (make-game loi empty t)]
        [else
         (make-game (compare-invaders loi lom)       ; fn-for-loi
                    (compare-missiles lom loi)       ; fn-for-lom
                    t)]))

;; ListOfInvader ListOfMissile -> ListOfInvader
;; Compares invaders with missiles, filtering those who collided: height(missile - invader) < 10
(check-expect (compare-invaders empty empty) empty)                                         ; base case
(check-expect (compare-invaders (list (make-invader 150 100 12)) empty)                     ; empty missiles list
              (list (make-invader 150 100 12)))
(check-expect (compare-invaders empty (list (make-missile 150 300)))                        ; empty missiles list
              empty)
(check-expect (compare-invaders (list I1 (make-invader 240 180 12)) (list M2))              ; missile impact invader
              (list (make-invader 240 180 12)))
(check-expect (compare-invaders (list I1) (list M2)) empty)
(check-expect (compare-invaders (cons (make-invader 80 400 10)                              ; no impact at all
                                      (cons (make-invader 180 256 -10) empty))
                                (list (make-missile 180 200) (make-missile 90 100)))
              (list (make-invader 80 400 10) (make-invader 180 256 -10)))
                            
;(define (compare-invaders loi lom) loi) ; stub

; <Template from loi with added list>
(define (compare-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (invader-hit? (first loi) lom)
             (compare-invaders (rest loi) lom)
             (cons (first loi) (compare-invaders (rest loi) lom)))]))
              
;; Invader ListOfMissile -> Boolean
;; produce true if y coord of invader is below and inside HIT-RANGE of any missile in list
;; ASSUMES: no empty lom as input

(check-expect (invader-hit? (make-invader 200 300 10) (list (make-missile 150 100) (make-missile 150 250)))  ; no hit
              false)
(check-expect (invader-hit? (make-invader 135 270 -10) (list (make-missile 134 280) (make-missile 150 30)))  ; hit
              true)
(check-expect (invader-hit? (make-invader 135 240 10) (list (make-missile 135 249) (make-missile 150 30)))   ; hit
              true)

;(define (invader-hit? i lom) false) ; stub

; <Template from lom with added compound>
(define (invader-hit? i lom)
  (cond [(empty? lom) false]
        [else
         (if (within-range? i (first lom))
             true
             (invader-hit? i (rest lom)))]))

;; Invader Missile -> Boolean
;; produce true if both x coord is same and y coord of missile is larger but within HIT-RANGE
(check-expect (within-range? (make-invader 23 56 10) (make-missile 76 56)) false)     ; not in y HIT-RANGE
(check-expect (within-range? (make-invader 45 78 -10) (make-missile 45 88)) true)     ; entered HIT-RANGE
(check-expect (within-range? (make-invader 76 34 10) (make-missile 76 43)) true)      ; inside hit range
(check-expect (within-range? (make-invader 62 39 10) (make-missile 62 39)) true)      ; same both coord

;(define (within-range? i m) false) ; stub

; <template from Invader with added compound>
(define (within-range? i m)
  (and (< (abs (- (invader-x i) (missile-x m))) 5)
       (<= (abs (- (invader-y i) (missile-y m))) 10)))

;; ListOfMissile ListOfInvader -> ListOfMissile
;; Compares missiles with invaders, filtering those who collided: height(missile - invader) < 10
(check-expect (compare-missiles empty empty) empty)                                         ; base case
(check-expect (compare-missiles empty (list (make-invader 150 100 12)))                     ; empty missiles list
              empty)
(check-expect (compare-missiles (list (make-missile 150 300) (make-missile 80 50)) empty)   ; empty invaders list
              (list (make-missile 150 300) (make-missile 80 50)))
(check-expect (compare-missiles (list M2 (make-missile 75 90))                              ; missile impact invader
                                (list I1 (make-invader 240 180 12)))
              (list (make-missile 75 90)))
(check-expect (compare-missiles (list (make-missile 180 200) (make-missile 90 100))         ; no impact at all
                                (cons (make-invader 80 400 10)
                                      (cons (make-invader 180 256 -10) empty)))
              (list (make-missile 180 200) (make-missile 90 100)))

;(define (compare-missiles lom loi) lom) ; stub

; <Template from lom with added list>
(define (compare-missiles lom loi)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [else
         (if (missile-hit? (first lom) loi)
             (compare-missiles (rest lom) loi)
             (cons (first lom) (compare-missiles (rest lom) loi)))]))

;; Missile ListOfInvader -> Boolean
;; produce true if y coord of missile is larger and inside HIT-RANGE of any invader in list
;; ASSUMES: no empty loi as input

(check-expect (missile-hit?  (make-missile 150 280) (list  (make-invader 200 300 10)           ; no hit
                                                           (make-invader 189 42 -10)))
              false)
(check-expect (missile-hit?  (make-missile 150 280) (list (make-invader 135 70 -10)            ; hit
                                                          (make-invader 149 270 10)))
              true)
(check-expect (missile-hit?  (make-missile 135 249) (list (make-invader 135 240 10)            ; hit
                                                          (make-invader 35 59 -10)))  
              true)

;(define (missile-hit? m loi) false) ; stub
; <Template from loi with added compound>
(define (missile-hit? m loi)
  (cond [(empty? loi) false]
        [else
         (if (within-range? (first loi) m)
             true
             (missile-hit? m (rest loi)))]))

;; ListOfMissile -> ListOfMissile
;; filter gone missiles from the list (height is < -10)
(check-expect (filter-missiles empty) empty)                    ; base case
(check-expect (filter-missiles (list (make-missile 10 40)       ; not gone missile
                                     (make-missile 20 50)
                                     (make-missile 10 -7)))
              (list (make-missile 10 40)
                    (make-missile 20 50)
                    (make-missile 10 -7)))
(check-expect (filter-missiles (list (make-missile 10 40)       ; gone missile
                                     (make-missile 20 50)
                                     (make-missile 10 -9)))
              (list (make-missile 10 40)
                    (make-missile 20 50)))

;(define (filter-missiles lom) lom) ; stub

(define (filter-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (isgone? (first lom))
             (filter-missiles (rest lom))
             (cons (first lom) (filter-missiles (rest lom))))]))

;; Missile -> Boolean
;; produce true is the y coord of missile is < -(MISSILE-HEIGHT / 2)

(check-expect (isgone? (make-missile 20 (- 0 (/ MISSILE-HEIGHT 2)))) false)  ; missile half-gone
(check-expect (isgone? (make-missile 78 -8)) true)                          ; missile gone

;(define (isgone? m) false) ; stub
(define (isgone? m)
  (<  (missile-y m) (- (/ MISSILE-HEIGHT 2))))

;; Game -> Game
;; produces the next state of the game, moving every part as corresponds
(check-expect (advance-game (make-game empty empty (make-tank 50 1)))                                 ; move tank right
              (make-game empty empty (make-tank (+ 50 TANK-SPEED) 1)))
(check-expect (advance-game (make-game empty empty (make-tank 50 -1)))                                ; move tank left
              (make-game empty empty (make-tank (- 50 TANK-SPEED) -1)))

(check-expect (advance-game (make-game (list (make-invader 100 150 10)) empty T1))                    ; inv going right
              (make-game (list (make-invader (+ 100 INVADER-X-SPEED) (+ 150 INVADER-Y-SPEED) 10))
                         empty
                         (make-tank 52 1)))
(check-expect (advance-game (make-game (list (make-invader 100 150 -10)) empty T1))                   ; inv going left
              (make-game (list (make-invader (- 100 INVADER-X-SPEED) (+ 150 INVADER-Y-SPEED) -10))
                         empty
                         (make-tank 52 1)))
(check-expect (advance-game (make-game empty (list (make-missile 30 240)) T2))                        ; missile and tank
              (make-game empty (list (make-missile 30 (- 240 MISSILE-SPEED))) (make-tank 48 -1)))     
(check-expect (advance-game (make-game (list (make-invader 34 85 10) (make-invader 89 75 -10))        ; move everything
                                       (list (make-missile 34 200) (make-missile 60 100))
                                       T2))
              (make-game (list (make-invader 35.5 86.5 10) (make-invader 87.5 76.5 -10))
                         (list (make-missile 34 190) (make-missile 60 90))
                         (make-tank 48 -1)))

;(define (advance-game gm) G0) ; stub

(define (advance-game gm)
  (make-game (generate-invader (advance-invaders (game-invaders gm)))
             (advance-missiles (game-missiles gm))
             (advance-tank (game-tank gm))))

;; ListOfInvader -> ListOfInvader
;; produce a new invader at random intervals

;(define (generate-invader loi) loi) ; stub
(define (generate-invader loi)
  (if (=  (random INVADE-RATE) 10)
      (cons (make-invader (random (+ WIDTH 1)) 0 10) loi)
      loi))

;; ListOfInvader -> ListOfInvader
;; advance the invaders accross the screen and generates a new invader randomly
(check-expect (advance-invaders empty) empty)                          ; base case
(check-expect (advance-invaders (list (make-invader 35 470 10)))       ; just move invader
              (list (make-invader 36.5 471.5 10)))
(check-expect (advance-invaders (list (make-invader WIDTH 470 10)))     ; change invader direction R-L
              (list (make-invader (- WIDTH INVADER-X-SPEED) 471.5 -10)))
(check-expect (advance-invaders (list (make-invader 0 470 -10)))        ; change invader direction L-R
              (list (make-invader 1.5 471.5 10)))

;(define (advance-invaders loi) loi) ; stub

(define (advance-invaders loi)
  (next-invaders (change-dir-invaders loi)))

;; ListOfInvader -> ListOfInvader
;; produces new coordinates of invaders in the list
(check-expect (next-invaders empty) empty)                                                   ; base case
(check-expect (next-invaders (list (make-invader 40 100 12)))                                ; inv going L-R
              (list (make-invader (+ 40 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 12)))
(check-expect (next-invaders (list (make-invader 40 100 -12)))                                ; inv going R-L
              (list (make-invader (- 40 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) -12)))

;(define (next-invaders loi) loi) ; stub

(define (next-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (next-invader (first loi))
               (next-invaders (rest loi)))]))

;; Invader -> Invader
;; produces an invader with new coordinates depending on dx
(check-expect (next-invader (make-invader 256 700 10))
              (make-invader (+ 256 INVADER-X-SPEED) (+ 700 INVADER-Y-SPEED) 10))  ; going right
(check-expect (next-invader (make-invader 256 700 -10))
              (make-invader (- 256 INVADER-X-SPEED) (+ 700 INVADER-Y-SPEED) -10)) ; going left

;(define (next-invader i) i) ; stub

(define (next-invader i)
  (cond [(< (invader-dx i) 0)
         (make-invader (- (invader-x i) INVADER-X-SPEED)
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]
        [(>= (invader-dx i) 0)
         (make-invader (+ (invader-x i) INVADER-X-SPEED)
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]))

;; ListOfInvader -> ListOfInvader
;; change direction of invaders when necessary
(check-expect (change-dir-invaders empty) empty)                            ; base case
(check-expect (change-dir-invaders (list (make-invader 0 50 -10)))          ; touch left going R-L
              (list (make-invader 0 50 10)))
(check-expect (change-dir-invaders (list (make-invader WIDTH 50 10)))       ; touch right going L-R
              (list (make-invader WIDTH 50 -10)))

;(define (change-dir-invaders loi) loi) ; stub

(define (change-dir-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (change-dir-invader (first loi))
               (change-dir-invaders (rest loi)))]))

;; Invader -> Invader
;; change direction of a single invader if it touches an edge of the screen
(check-expect (change-dir-invader (make-invader 50 50 10)) (make-invader 50 50 10))          ; no dir change
(check-expect (change-dir-invader (make-invader 0 50 -10)) (make-invader 0 50 10))           ; from R-L to L-R
(check-expect (change-dir-invader (make-invader WIDTH 50 10)) (make-invader WIDTH 50 -10))   ; from L-R to R-L

;(define (change-dir-invader i) i) ; stub

(define (change-dir-invader i)
  (cond [(and (<= (invader-x i) 0) (<= (invader-dx i) 0))
         (make-invader 0 (invader-y i) (- (invader-dx i)))]
        [(and (>= (invader-x i) WIDTH) (>= (invader-dx i) 0))
         (make-invader WIDTH (invader-y i) (- (invader-dx i)))]
        [else i]))

;; ListOfMissile -> ListOfMissile
;; decreases every missile y-coord in the list by MISSILE-SPEED
(check-expect (advance-missiles empty) empty)                                     ;base case
(check-expect (advance-missiles (list (make-missile 50 100)))
              (list (make-missile 50 (- 100 MISSILE-SPEED))))
(check-expect (advance-missiles (list (make-missile 50 100) (make-missile 40 80)))
              (list (make-missile 50 (- 100 MISSILE-SPEED)) (make-missile 40 70)))

;(define (advance-missiles lom) lom) ; stub

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (advance-missile (first lom))
               (advance-missiles (rest lom)))]))

;; Missile -> Missile
;; move a single missile up on its y-coord by MISSILE-SPEED units
(check-expect (advance-missile (make-missile 40 70)) (make-missile 40 60))
(check-expect (advance-missile (make-missile 100 0)) (make-missile 100 -10))

;(define (advance-missile m) m) ; stub

(define (advance-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
;; moves the tank left or right depending on its dir
(check-expect (advance-tank T1) (make-tank 52 1))                      ; going right
(check-expect (advance-tank T2) (make-tank 48 -1))                     ; going left
(check-expect (advance-tank (make-tank 0 -1)) (make-tank 2 1))         ; touch left border going left
(check-expect (advance-tank (make-tank WIDTH 1)) (make-tank (- WIDTH 2) -1)) ; touch right border going right

;(define (advance-tank t) t)         ; stub

(define (advance-tank t)
  (next-tank (change-direction t)))

;; Tank -> Tank
;; moves the tank left or right SPEED units depending on its dir
(check-expect (next-tank T1) (make-tank 52 1))         ; going right
(check-expect (next-tank T2) (make-tank 48 -1))         ; going left              

;(define (next-tank t) t) ; stub

(define (next-tank t)
  (cond [(= (tank-dir t) -1) (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))]
        [(= (tank-dir t) 1) (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))]))

;; Tank -> Tank
;; change direction of tank whenever it touches left or right edge of screen
(check-expect (change-direction T1) T1)
(check-expect (change-direction T2) T2)
(check-expect (change-direction (make-tank 0 -1)) (make-tank 0 1))         ; touch left border going left
(check-expect (change-direction (make-tank WIDTH 1)) (make-tank WIDTH -1)) ; touch right border going right

;(define (change-direction t) t) ; stub

(define (change-direction t)
  (cond [(and (<= (tank-x t) 0) (= (tank-dir t) -1))
         (make-tank 0 1)]
        [(and (>= (tank-x t) WIDTH) (= (tank-dir t) 1))
         (make-tank WIDTH -1)]
        [else t]))

;; Game -> Image
;; render an image with the current state of the game

(check-expect (render-game G0) (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BCKG))  ; render tank
(check-expect (render-game (make-game empty (list (make-missile 67 200)) T0))
              (place-image MISSILE 67 200                                                     ; render missile and tank
                           (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BCKG)))
(check-expect (render-game (make-game (list (make-invader 200 300 10))
                                      (list (make-missile 67 200))
                                      T0))
              (place-image INVADER 200 300                                                    ; render everything
                           (place-image MISSILE 67 200
                                        (place-image TANK
                                                     (/ WIDTH 2)
                                                     (- HEIGHT TANK-HEIGHT/2)
                                                     BCKG))))

;(define (render-game gm) BCKG) ; stub

(define (render-game gm)
  (overlay (render-invaders (game-invaders gm))
           (render-missiles (game-missiles gm))
           (render-tank (game-tank gm))
           BCKG))

;; ListOfInvader -> Image
;; places a list of invader images onto BACKGROUND-TRANSPARENT
(check-expect (render-invaders (list (make-invader 50 80 10) (make-invader 100 200 -10)))
              (place-image INVADER 50 80
                           (place-image INVADER 100 200 BACKGROUND-TRANSPARENT)))

;(define (render-invaders loi) BACKGROUND-TRANSPARENT) ;stub

(define (render-invaders loi)
  (cond [(empty? loi) BACKGROUND-TRANSPARENT]
        [else
         (render-invader-on (first loi) (render-invaders (rest loi)))]))

;; Invader Image -> Image
;; put a single invader onto img at invader coordinates
(check-expect (render-invader-on (make-invader 60 90 10) BACKGROUND-TRANSPARENT)
              (place-image INVADER 60 90 BACKGROUND-TRANSPARENT))

;(define (render-invader-on i img) BACKGROUND-TRANSPARENT) ; stub

; took template from Invader with added atomic parameter
(define (render-invader-on i img)
  (place-image INVADER (invader-x i) (invader-y i) img))

;; ListOfMissile -> Image
;; places a list of missiles onto BACKGROUND-TRANSPARENT
(check-expect (render-missiles (list (make-missile 50 80) (make-missile 100 200)))
              (place-image MISSILE 50 80
                           (place-image MISSILE 100 200 BACKGROUND-TRANSPARENT)))

;(define (render-missiles lom) BACKGROUND-TRANSPARENT) ;stub

(define (render-missiles lom)
  (cond [(empty? lom) BACKGROUND-TRANSPARENT]
        [else
         (render-missile-on (first lom) (render-missiles (rest lom)))]))

;; Missile Image -> Image
;; put a single missile onto img at missile coordinates

(check-expect (render-missile-on (make-missile 60 90) BACKGROUND-TRANSPARENT)
              (place-image MISSILE 60 90 BACKGROUND-TRANSPARENT))

;(define (render-missile-on m img) BACKGROUND-TRANSPARENT) ; stub

; took template from Missile with added atomic parameter
(define (render-missile-on m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))


;; Tank -> Image
;; places the tank onto BACKGROUND-TRANSPARENT
(check-expect (render-tank T0) (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND-TRANSPARENT))

;(define (render-tank t) BACKGROUND-TRANSPARENT) ;stub

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND-TRANSPARENT)) 



;; Game KeyEvent -> Game
;; produces the reaction of the game when several keys are pressed
;; space-bar: fires a rocket
;; left arrow: makes tank move to the left
;; right arrow: makes tank move to the right
(check-expect (handle-keys (make-game empty empty T1) "left")                       ; left arrow
              (make-game empty empty (make-tank 50 -1)))
(check-expect (handle-keys (make-game empty empty T2) "right")                      ; right arrow
              (make-game empty empty (make-tank 50 1)))
(check-expect (handle-keys (make-game empty empty T0) " ")                          ; space bar
              (make-game empty
                         (list (make-missile (/ WIDTH 2) (- HEIGHT 24)))
                         T0))              

;(define (handle-keys gm a-key) G0) ; stub

(define (handle-keys gm a-key)
  (cond
    [(or (key=? a-key "left") (key=? a-key "right"))
     (make-game (game-invaders gm)
                (game-missiles gm)
                (turn-tank (game-tank gm) a-key))]
    [(key=? a-key " ")
     (make-game (game-invaders gm)
                (fire-missile (game-missiles gm) (tank-x (game-tank gm)))
                (game-tank gm))]
    [else gm]))

;; Tank KeyEvent -> Game
;; turns the tank if needed, signaled by the given key event
(check-expect (turn-tank T1 "left") (make-tank 50 -1))  ; R-L to L-R
(check-expect (turn-tank T2 "right") (make-tank 50 1))  ; L-R to R-L
(check-expect (turn-tank T1 "right") T1) ; going right, no change
(check-expect (turn-tank T2 "left") T2)  ; going left, no change

;(define (turn-tank t a-key) t) ; stub

(define (turn-tank t a-key)
  (cond [(and (key=? a-key "left") (> (tank-dir t) 0))
         (make-tank (tank-x t) (- (tank-dir t)))]
        [(and (key=? a-key "right") (< (tank-dir t) 0))
         (make-tank (tank-x t) (abs (tank-dir t)))]
        [else t]))

;; ListOfMissile Natural -> ListOfMissile
;; adds a new missile to the list at given x-coord and (HEIGHT - 24) y-coord
(check-expect (fire-missile empty 250)
              (list (make-missile 250 (- HEIGHT 24))))
(check-expect (fire-missile (list (make-missile 50 45)) 78)
              (list (make-missile 78 (- HEIGHT 24)) (make-missile 50 45)))              

;(define (fire-missile lom x)) lom) ; stub
(define (fire-missile lom x)
  (cons (make-missile x (- HEIGHT 24)) lom))

;; Game -> Boolean
;; produce true if an invader reached the ground
(check-expect (touch-ground? (make-game (list (make-invader 35 HEIGHT -10)) empty T0))
              true)
(check-expect (touch-ground? (make-game (list (make-invader 35 (- HEIGHT 1) 10)) empty T0))
              false)

;(define (touch-ground? gm) false) ; stub

(define (touch-ground? gm)
  (check-invaders? (game-invaders gm)))

;; ListOfInvaders -> Boolean
;; goes through the list checking if height of an invader reached the ground
(check-expect (check-invaders? (list (make-invader 35 HEIGHT -10)))
              true)
(check-expect (check-invaders? (list (make-invader 35 (- HEIGHT 1) 10)))
              false)

;(define (check-invaders? loi) false)

(define (check-invaders? loi)
  (cond [(empty? loi) false]
        [else
         (if (reached-ground? (first loi))
             true
             (check-invaders? (rest loi)))]))
  
;; Invader -> Boolean
;; check y cord of an invader to check if touched ground
(check-expect (reached-ground? (make-invader 35 HEIGHT 10)) true)
(check-expect (reached-ground? (make-invader 35 (- HEIGHT 1) -10)) false)

;(define (reached-ground? i) false) ; stub
(define (reached-ground? i)
  (>= (invader-y i) HEIGHT))

;; Game -> Image
;; produce an image of the current state of the game, with another background
(check-expect (render-game-over G0)
              (place-image TANK (tank-x (game-tank G0)) (- HEIGHT TANK-HEIGHT/2) BCKG-GAME-OVER))
(check-expect (render-game-over (make-game (list (make-invader 100 HEIGHT 10)
                                                 (make-invader 150 300 -10))
                                           (list (make-missile 200 300)
                                                 (make-missile (/ WIDTH 2) 0))
                                           (make-tank 80 1)))
              (overlay (place-image INVADER 100 HEIGHT
                                    (place-image INVADER 150 300 BACKGROUND-TRANSPARENT))
                       (place-image MISSILE 200 300
                                    (place-image MISSILE (/ WIDTH 2) 0 BACKGROUND-TRANSPARENT))
                       (place-image TANK 80 (- HEIGHT TANK-HEIGHT/2) BACKGROUND-TRANSPARENT)
                       BCKG-GAME-OVER))
                                           
;(define (render-game-over gm) BCKG-GAME-OVER) ; stub
(define (render-game-over gm)
  (overlay (render-invaders (game-invaders gm))
           (render-missiles (game-missiles gm))
           (render-tank (game-tank gm))
           BCKG-GAME-OVER))
