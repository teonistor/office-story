#lang racket

(struct game-state (inventory room-contents room-doors location misc))
;(struct room (description doors fixed-objects))

(define (inventory-remove inventory object)
  (and (set-member? inventory object)
       (set-remove inventory object)))

(define (room-add room-contents room object)
    (dict-update room-contents room (λ (given-room-contents) (set-add given-room-contents object)) '()))

(define (room-remove room-contents room object)
  (let ([given-room-contents (dict-ref room-contents room '())])
    (and (set-member? given-room-contents object)
         (dict-set room-contents room (set-remove given-room-contents object)))))

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

(define (cannot-take object) ; TODO tell why
  (display "You cannot pick that up.\n"))


(define (look-enumerate unknown-contents line-three)
  (match (length unknown-contents)
    [0 line-three]
    [1 (string-append line-three (car unknown-contents) ".")]
    [_ (look-enumerate (cdr unknown-contents) (string-append line-three (car unknown-contents) ", "))]))

(define (look-known-contents location contents unknown-contents line-two)
  (if (empty? contents)
      (look-enumerate unknown-contents (string-append line-two "Also in this room: "))
      (match (list location (car contents))
        [(list 'office-right-0 "banana") (look-known-contents location (cdr contents) unknown-contents (string-append line-two "There is no one here now, but someone must have been recently because they left a banana on a desk. "))]
        [(list 'cafe "coffee")           (look-known-contents location (cdr contents) unknown-contents (string-append line-two "There is a cup of coffee on the counter. "))]
        [_ (look-known-contents location (cdr contents) (cons (car contents) unknown-contents) line-two)])))

(define (look game)
  (let* ([location (game-state-location game)]
         [line-one
          (match location
            ['outside "You are in the plaza outside a large but secluded office building. There is a door to go inside, and an alley heading to the car park. "]
            ['car-park "The employees' car park is packed. Some cars look very expensive. "]
            ['reception "You are in the reception area of the office. To your left and right are barriers which require a card to access. Behind you are doors leading outside. In front of you is a reception desk. "]

            ['stair-right-0 "A bland-looking staircase, with stairs going up. In front of you is a door to the ground level office. "]
            ['stair-right-1 "A bland-looking staircase, with stairs going up and down. In front of you is the level 1 workspace. Behind you are the barriers to reception. "]
            ['stair-right-2 "A bland-looking staircase, with stairs going down. In front of you is a door to the level 2 workspace. "]
            ['office-right-0 "You are in the smallest office you could imagine in such a large building. It's tight and has no windows. You have the staircase behind you and an obscure door with a fingerprint sacnner to your right. "]
            ['office-right-1 "You are on the level 1 workspace which is vast and full of people at work. You have the staircase behind you and an obscure door with a fingerprint sacnner to your right. "]

            [_ "You are in a totally empty room. This is likely because the game creator misplaced a door or made a typo or something. You shouldn't be here."])]

         [line-two (look-known-contents location (set->list (dict-ref (game-state-room-contents game) location '())) '() "")])
    (display line-one)
    (display line-two)
    game))
 
      

(define (go game door)
  ; TODO
  game)

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

(define (talk game person)
  ; TODO
  game)

(define (give game person object)
  ; TODO
  game)


(define (bad-input game)
  (begin
    (display "You don't quite know ow to do that.")
    game))

;(game-state-inventory (take (game-state '() (hash "rec" (set "chair" "desk")) "rec" '()) "chair"))
;(game-state-inventory (take (game-state '() (hash "rec" (set "chair" "desk")) "rec" '()) "desk"))
;(game-state-inventory (take (game-state '() (hash "rec" (set "chair" "desk")) "rec" '()) "hat"))
;(game-state-inventory (take (game-state '() (hash "rec" (set "chair" "desk")) "else" '()) "chair"))


(define (check-exit)
  (begin
    (display "Really exit? (y/n) ")
    (let ([input (read-line)])
      (or (not (string? input)) (string=? "y" (string-downcase input))))))

(define (parse-input input) ; TODO
  (match input
    [ "exit" #f]
    [ "look" look]
    [(regexp #rx"^go( to| through| via|)( the)? (.+)$" (list _ _ _ where)) (λ (game) (go game where))]
    [(regexp #rx"^(take|pick up)( the)? (.+)$" (list _ _ _ what))          (λ (game) (take game what))]
    [(regexp #rx"^talk( to the| to|) (.+)$" (list _ _ who))                (λ (game) (talk game who))]
    [(regexp #rx"^give( the)? (.+) to( the)? (.+)$" (list _ _ what _ who)) (λ (game) (give game who what))]
    [(regexp #rx"^give( the)? (.+)( the)? (.+)$" (list _ _ who _ what))    (λ (game) (give game who what))]
    [_ bad-input]))

(define (main-loop game)
  (let* ([input (read-line)]
         [action (and (string? input) (parse-input input))])
    (if action
        (main-loop (action game))
        (if (check-exit)
            game
            (main-loop game)))))

(define gamee (game-state
;(main-loop (game-state
            '() ; inventory
            (hash 'office-right-0 (set "banana")) ; room contents

            ; doors
            (hash 'outside (hash "door" 'reception "alley" 'car-park)
                  'reception (hash "right" 'stair-right-0))

            'office-right-0 ; starting location
            '() ; no special state to begin with
            ))

; (struct game-state (inventory room-contents room-descriptions room-doors location missions))

(look gamee)



