#lang racket

(struct game-state (inventory room-contents location missions))
(struct room (description doors fixed-objects))

(define (parse-input-new input) ; TODO
  (match input
    [ "exit" 'exit]
    [(regexp #rx"^go(| to| through| via)(| the) (.+)$" (list _ _ _ what)) 'go]
    [(regexp #rx"^take(| the) (.+)$" (list _ _ what)) 'take]
    [_ 'huh]))
    
; (define (look room

(define (inventory-remove inventory object)
  (and (set-member? inventory object)
       (set-remove inventory object)))

(define (room-add room-contents room object)
    (dict-update room-contents room (λ (given-room-contents) (set-add given-room-contents object)) '()))

(define (room-remove room-contents room object)
  (let ([given-room-contents (dict-ref room-contents room '())])
    (and (set-member? given-room-contents object)
         (dict-set room-contents room (set-remove given-room-contents object)))))

(define (cannot-take object) ; TODO tell why
  (display (string-append "You cannot pick up the " object ".\n")))

(define (take game object)
  (let ([updated-room-contents (room-remove (game-state-room-contents game) (game-state-location game) object)])
    (if updated-room-contents
        (begin
          (display (string-append "You pick up the " object ".\n"))
          (let ([updated-inventory (set-add (game-state-inventory game) object)])
            (struct-copy game-state game [inventory updated-inventory][room-contents updated-room-contents])))
        (begin
          (cannot-take object)
          game))))

(define (achieve-mission missions mission)
  (if (member mission missions)
      missions
      (begin
        (display " *** Mission achieved ***\n ")
        (display (match mission
                   ['creator "Use the creator's name to gain access to the office"]
                   [_ "<error>"]))
        (display "\n [")
        (display (+ 1 (length missions)))
        (display "/3]\n")
        (cons mission missions))))


;(game-state-inventory (take (game-state '() (hash "rec" (set "chair" "desk")) "rec" '()) "chair"))
;(game-state-inventory (take (game-state '() (hash "rec" (set "chair" "desk")) "rec" '()) "desk"))
;(game-state-inventory (take (game-state '() (hash "rec" (set "chair" "desk")) "rec" '()) "hat"))
;(game-state-inventory (take (game-state '() (hash "rec" (set "chair" "desk")) "else" '()) "chair"))
         
         
         
         

;;;; Old

;(define (check-exit)
;  (begin
;    (display "Really exit? (y/n) ")
;    (let ([input (read-line)])
;      (if (or (not (string? input)) (string=? "y" (string-downcase input)))
;          (void)
;          (process-input)))))

;(define (process-input)
;  (let ([input (read-line)])
;    (if (string? input)
;      (if (string=? "exit" (string-downcase input))
;         (check-exit)
;         (begin
;          (dispatch-command (string-split (string-downcase input)))
;          (newline)
;          (process-input)))
;      (check-exit))))

