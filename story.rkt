#lang racket

(struct game-state (inventory room-contents location misc))
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

(define (achieve-achievement game achievement) ; TODO use misc
  (let ([achievements (dict-ref (game-state-misc game) 'achievements '())])
    (if (member achievement achievements)
        game
        (begin
          (display " *** Achievement unlocked ***\n *** ")
          (display (match achievement
                   ['creator "Use the creator's name to gain access to the office building"]
                   ['coffee "Help an overworked employee wake up"]
                   ['janitor "Clean up somebody else's mess"]
                   [_ "<error>"]))
          (display "\n *** [")
          (display (+ 1 (length achievements)))
          (display "/3] ***\n")
          (struct-copy game-state game [misc (dict-set (game-state-misc game) 'achievements (cons achievement achievements))])))))

(define (cannot-take object) ; TODO tell why
  (display "You cannot pick that up.\n"))


(define (look-enumerate unknown-contents line-three)
  (match (length unknown-contents)
    [0 line-three]
    [1 (string-append line-three (car unknown-contents) ".")]
    [_ (look-enumerate (cdr unknown-contents) (string-append line-three (car unknown-contents) ", "))]))

(define (look-known-contents location contents unknown-contents line-two)
  (if (empty? contents)
      (if (empty? unknown-contents)
          line-two
          (look-enumerate unknown-contents (string-append line-two "Also in this room: ")))
      (match (list location (car contents))
        [(list 'office-right-0 "banana")       (look-known-contents location (cdr contents) unknown-contents (string-append line-two "There is no one here now, but someone must have been recently because they left a banana on a desk. "))]
        [(list 'office-right-2 "notebook")     (look-known-contents location (cdr contents) unknown-contents (string-append line-two "One employee left an open notebook on his desk. "))]
        [(list 'office-right-2 "badge")        (look-known-contents location (cdr contents) unknown-contents (string-append line-two "Another, his badge. "))]
        [(list 'office-right-2 "candy wrapper")(look-known-contents location (cdr contents) unknown-contents (string-append line-two "Another, a candy wrapper. "))]

        [(list 'cafe "coffee")  (look-known-contents location (cdr contents) unknown-contents (string-append line-two "There is a cup of coffee on the counter. "))]
        [(list 'restroom "key") (look-known-contents location (cdr contents) unknown-contents (string-append line-two "Looking around, you notice a key somebody must have dropped in one of the stalls. "))]

        [(list 'office-left-1 'mess)       (look-known-contents location (cdr contents) unknown-contents (string-append line-two "There is a huge mess of plastic cups, bottles, and bits of paper in the middle of the floor, which the poor cleaner is struggling to pick up one by one. "))]
        [(list 'office-left-2 "hard drive")(look-known-contents location (cdr contents) unknown-contents (string-append line-two "A hard drive sits in a tray on the desk. "))]
        [(list 'broom-closet "broom")      (look-known-contents location (cdr contents) unknown-contents (string-append line-two "A broom is leaning against the corner amongst other things. "))]
        [(list 'broom-closet "badge")      (look-known-contents location (cdr contents) unknown-contents (string-append line-two "A badge is hanging from a hook in the wall. "))]

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
            ['office-right-1 "You are on the level 1 workspace which is vast and full of people at work. You have the staircase behind you. All the way across the floor is the office cafe. "]
            ['office-right-2 "You are on the level 2 workspace which is as vast as the one below but pretty deserted. The staircase is behind you and a door to the restrooms is in front. One employee is sitting with his head on the desk, perhaps asleep or at any rate very tired. Everybody else seems to have left in a hurry. "]
            ['cafe "Some music is playing softly in the cafe and a few groups of people are sat at tables, chatting. The noise from the workspace behind you is inaudible. A barista is rummaging behind the counter. "]
            ['restroom "The restroom looks very standard and smells slightly above average. "]

            ['stair-left-0 "A colorful staircase, with stairs going up. In front of you are numerous locked doors, the only unlocked one being the broom closet. "]
            ['stair-left-1 "A colorful staircase, with stairs going up and down. In front of you is a fancy door, ajar, with a sign reading 'Ideation Hub'. Behind you are the barriers to reception. "]
            ['stair-left-2 "A colorful staircase, with stairs going down. In front of you is a door with a sign reading 'Execdir'. "]
            ['office-left-1 "The so-called Ideation Hub you find yourself in consists of a whole disarray of chairs and desks. It looks like someone threw a mad party in here rather than using the place to come up with ideas. "]
            ['office-left-2 "You find yourself in the Executive Director's office, which is posh and very well lit. You could really use that leather chair for your day-to-day desk work! "]
            ['broom-closet "There is an array of cleaning substances in the dimly lit broom closet. "]
            
            [_ "You are in a totally empty room. This is likely because the game creator misplaced a door or made a typo or something. You shouldn't be here."])]

         [line-two (look-known-contents location (set->list (dict-ref (game-state-room-contents game) location '())) '() "")])
    (display line-one)
    (display line-two)
    (newline)
    game))
 
      

(define (go game where)
  
  (match (list (game-state-location game) where)
    [(list 'outside (regexp #rx"door|in|inside")) "outside go in"]
    [(list 'outside "alley") "outside go park"]
    
    [(list 'car-park "alley") "park go front"]
    
    [(list 'reception (regexp #rx"door|back")) "reception go back"]
    [(list 'reception "left") "reception go left"]
    [(list 'reception "right") "reception go right"]
    
    [(list 'car-park "alley") "park go front"]))

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

(define (safe-input)
  (let ([input (read-line)])
    (and (string? input) (string-downcase input))))

(define (talk-to-lady game n)
 (if (zero? (dict-ref (game-state-misc game) 'lady-status 0))
  (match n
    [0 (begin (display "You approach the reception desk and the lady looks up at you.\nYou: Good morning!\nLady: Hello, how can I help you?\nYou: I am... ")
              (talk-to-lady game 1))]
    [1 (begin (display "(cleaner/visitor/employee) ")(match (safe-input)
         ["cleaner" (begin (display "You: I am the new cleaner supposed to start today, but they haven't given me a badge yet. Could you let me in and point me where to go?\nLady: Sure. What's your name?\nYou: ") (talk-to-lady game 12))]
         ["visitor" (begin (display "You: I'm visiting somebody for a business meeting.\nLady: Excellent! Could you give your name, sir?\nYou: ") (talk-to-lady game 22))]
         ["employee"(begin (display "You: I work here but I... uh... forgot my badge today.\nLady: Oh, that's a shame. Can I have your name?\nYou: ") (talk-to-lady game 32))]
         [_ (talk-to-lady game 1)]))]
    [12 (begin (read-line)
               (display "Lady: Great. Head over to your left and downstairs and you'll find everything you need.\nAs the left barrier opens, the telephone rings and the lady picks it up.\n")
               (struct-copy game-state game [misc (dict-set (game-state-misc game) 'lady-status 1)]))]
    [22 (begin (read-line)
               (display "Lady: Great. And who are you visiting?\nYou: ")
               (talk-to-lady game 23))]
    [23 (match (safe-input)
          ["teo nistor" (begin (display "Lady: Good. I'll open the barriers on your right for you. Head through there and you will probably find him upstairs.\n")
                               (achieve-achievement (struct-copy game-state game [misc (dict-set (game-state-misc game) 'lady-status 2)]) 'creator))]
          [_ (begin (display "The lady fiddles on her computer for a while.\nLady: Sorry, it looks like no one with that name works here.\n")(talk-to-lady game 1))])]
    [32 (match (safe-input)
          ["teo nistor" (begin (display "Lady: Great! Here, I'll give you this temporary badge. Don't forget to return it at the end of the day.\n")
                               (achieve-achievement (struct-copy game-state game [misc (dict-set (game-state-misc game) 'lady-status 3)]) 'creator))]
          [_ (begin (display "The lady fiddles on her computer for a long time.\nLady: Sorry, I can't find your name in the system.\n")(talk-to-lady game 1))])])
  (begin
    (display "The lady doesn't pay you any attention.\n")
    game)))
          
        

(define (talk game person)
  (match (list (game-state-location game) person)
    [(list 'reception "lady") (talk-to-lady game 0)]
; TODO employee?

    [_ (begin (display "You open your mouth to say something, but no words come to mind.")
              game)]))

     
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

            ; room contents
            (hash 'office-right-0 (set "banana")
                  'office-right-2 (set "notebook" "candy wrapper")
                  'office-left-1 (set "pencil" 'mess)
                  'office-left-2 (set "hard drive" "folder" "charger")
                  'broom-closet (set "broom"))

            ; doors
            ;(hash 'outside (hash "door" 'reception "alley" 'car-park)
             ;     'reception (hash "right" 'stair-right-0))

            'cafe ; starting location
            (hash) ; no special state to begin with
            ))

; (struct game-state (inventory room-contents room-descriptions room-doors location missions))

(look gamee)


; TODO (define (entry-point)

