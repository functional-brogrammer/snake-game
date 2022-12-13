;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |snake game|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; a world is [make-world number number movement]
(define-struct world (x-posn y-posn movement))

;; a movement is one of the following:
;; - "up"
;; - "down"
;; - "left"
;; - "right"

(define INITIAL-WORLD (make-world 250 250 "right"))

(define SNAKE (rectangle 30 30 "solid" "forest green"))
(define FOOD (circle 10 "solid" "yellow"))
(define WORLD-WIDTH 500)
(define WORLD-HEIGHT 500)


; detecting collision with the walls to prevent out of bounds.

;; draw the world. the world is the snake.
;; world -> image
(define (draw-world w)
  (place-image
   FOOD
   250 250
   (place-image
    SNAKE
    (world-x-posn w) (world-y-posn w)
    (empty-scene WORLD-WIDTH WORLD-HEIGHT)))
  )
  

(check-expect (draw-world (make-world 30 250 "left"))
              (place-image SNAKE
                           30 250
                           (empty-scene WORLD-WIDTH WORLD-HEIGHT)))
(check-expect (draw-world (make-world 501 250 "right"))
              (place-image SNAKE
                           500 250
                           (empty-scene WORLD-WIDTH WORLD-HEIGHT)))
                         
; speed in pixels per second
(define BASE-SPEED 300)
;;frame interval
(define FRAME-INTERVAL 1/30)


;; update the world for passage of time
;; world -> world
(define (tock w)
  (cond [(string=? (world-movement w) "left")
        (make-world (- (world-x-posn w)(* FRAME-INTERVAL BASE-SPEED))
                    (world-y-posn w)
                    (world-movement w))]
        [(string=? (world-movement w) "right")
        (make-world (+ (world-x-posn w)(* FRAME-INTERVAL BASE-SPEED))
                    (world-y-posn w)
                    (world-movement w))]
        [(string=? (world-movement w) "up")
         (make-world (world-x-posn w)
         (- (world-y-posn w)(* FRAME-INTERVAL BASE-SPEED))
         (world-movement w))]
        [(string=? (world-movement w) "down")
         (make-world (world-x-posn w)
          (+ (world-y-posn w)(* FRAME-INTERVAL BASE-SPEED))
          (world-movement w))]
        [(= (world-x-posn  0)) (make-world (world-x-posn w)(world-y-posn) "up")]
        [(= (world-x-posn  500)) (make-world (world-x-posn w)(world-y-posn) "down")]
        [(= (world-y-posn  0)) (make-world (world-x-posn w)(world-y-posn) "right")]
        [(= (world-y-posn  500)) (make-world (world-x-posn w)(world-y-posn) "left")]
        )
  )

 
(check-expect (tock (make-world 250 250 "left"))
              (make-world (- 250 (* FRAME-INTERVAL BASE-SPEED))
                          250
                          "left"))

;; handle a key event
;; world key-event -> world
(define (keh w ke)
  (cond [(string=? ke "left")
         (make-world (world-x-posn w)(world-y-posn w) "left")]
        [(string=? ke "right")
         (make-world (world-x-posn w)(world-y-posn w) "right")]
        [(string=? ke "up")
         (make-world (world-x-posn w)(world-y-posn w) "up")]
        [(string=? ke "down")
         (make-world (world-x-posn w)(world-y-posn w) "down")]
        [else w]))

  
(check-expect (keh (make-world 250 250 "left") "right")
              (make-world 250 250 "right"))
 
(big-bang INITIAL-WORLD
  [to-draw draw-world]
  [on-tick tock]
  [on-key keh])