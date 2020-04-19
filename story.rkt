#lang racket

(struct game-state (inventory room-contents location misc))

(define (inventory-remove inventory object)
  (and (set-member? inventory object)
       (set-remove inventory object)))

(define (room-add room-contents room object)
    (dict-update room-contents room (λ (given-room-contents) (set-add given-room-contents object)) '()))

(define (room-remove room-contents room object)
  (let ([given-room-contents (dict-ref room-contents room '())])
    (and (set-member? given-room-contents object)
         (dict-set room-contents room (set-remove given-room-contents object)))))

(define (achieve-achievement game achievement)
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
            ['car-park "The employees' car park is packed. Some cars look very expensive. There is an alley going forward to the building. "]
            ['reception "You are in the reception area of the office. To your left and right are barriers which require a card to access. Behind you are doors leading outside. In front of you is a reception desk with a lady seating behind it. "]

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

(define (inventory-contains game what)
  (member what (game-state-inventory game)))

(define (go-specific game where)
  (look (struct-copy game-state game [location where])))

(define (go game where)
  (match (list (game-state-location game) where)
    [(list 'outside (regexp #rx"door|in|inside|forward")) (go-specific game 'reception)]
    [(list 'outside "alley") (go-specific game 'car-park)]
    
    [(list 'car-park (regexp #rx"alley|forward")) (go-specific game 'outside)]
    
    [(list 'reception (regexp #rx"door|back")) (go-specific game 'outside)]
    [(list 'reception "left") (if (or (= 1 (dict-ref (game-state-misc game) 'lady-status 0))(inventory-contains game "badge"))
                                  (go-specific game 'stair-left-1)
                                  (begin (display "The barriers are shut so you cannot go through.\n") game))]
    [(list 'reception "right") (if (or (= 2 (dict-ref (game-state-misc game) 'lady-status 0))(inventory-contains game "badge"))
                                   (go-specific game 'stair-right-1)
                                   (begin (display "The barriers are shut so you cannot go through.\n") game))]

    [(list 'stair-right-0 (regexp #rx"door|office")) (go-specific game 'office-right-0)]
    [(list 'stair-right-0 "up") (go-specific game 'stair-right-1)]
    [(list 'stair-right-1 (regexp #rx"forward|workspace")) (go-specific game 'office-right-1)]
    [(list 'stair-right-1 "down") (go-specific game 'stair-right-0)]
    [(list 'stair-right-1 "up") (go-specific game 'stair-right-2)]
    [(list 'stair-right-1 "back") (go-specific game 'reception)]
    [(list 'stair-right-2 (regexp #rx"forward|workspace|door")) (go-specific game 'office-right-2)]
    [(list 'stair-right-2 "down") (go-specific game 'stair-right-1)]

    [(list 'office-right-0 "back") (go-specific game 'stair-right-0)]
    [(list 'office-right-0 (regexp #rx"right|door")) (begin (display "You touch your finger on the fingerprint scanner but it denies access.\n") game)]
    [(list 'office-right-1 "back") (go-specific game 'stair-right-1)]
    [(list 'office-right-1 (regexp #rx"forward|cafe")) (go-specific game 'cafe)]
    [(list 'office-right-2 "back") (go-specific game 'stair-right-2)]
    [(list 'office-right-2 (regexp #rx"forward|door")) (go-specific game 'restroom)]

    [(list 'cafe (regexp #rx"back|workspace")) (go-specific game 'office-right-1)]
    [(list 'restroom (regexp #rx"back|workspace")) (go-specific game 'office-right-2)]

    [(list 'stair-left-0 "door") (go-specific game 'broom-closet)]
    [(list 'stair-left-0 "up") (go-specific game 'stair-left-1)]
    [(list 'stair-left-1 (regexp #rx"forward|door")) (go-specific game 'office-left-1)]
    [(list 'stair-left-1 "down") (go-specific game 'stair-left-0)]
    [(list 'stair-left-1 "up") (go-specific game 'stair-left-2)]
    [(list 'stair-left-1 "back") (go-specific game 'reception)]
    [(list 'stair-left-2 "down") (go-specific game 'stair-left-1)]
    [(list 'stair-left-2 (regexp #rx"forward|door")) (if (inventory-contains game "key")
                                                         (go-specific game 'office-left-2)
                                                         (begin (display "The door is locked.\n") game))]

    [(list 'broom-closet "back") (go-specific game 'stair-left-0)]
    [(list 'office-left-1 "back") (go-specific game 'stair-left-1)]
    [(list 'office-left-2 "back") (go-specific game 'stair-left-2)]

    [_ (begin (display "You cannot go that way.\n") game)]))

(define (take-special game object)
  (match object
    ["hard drive" (mission-accomplished game)]
    [_ game]))

(define (take game object)
  (let ([updated-room-contents (room-remove (game-state-room-contents game) (game-state-location game) object)])
    (if updated-room-contents
        (begin
          (display (string-append "You pick up the " object ".\n"))
          (let ([updated-inventory (set-add (game-state-inventory game) object)])
            (take-special (struct-copy game-state game [inventory updated-inventory][room-contents updated-room-contents]) object)))
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
               (struct-copy game-state game [misc (dict-set (game-state-misc game) 'lady-status 1)][room-contents (room-add (game-state-room-contents game) 'broom-closet "badge")]))]
    [22 (begin (read-line)
               (display "Lady: Great. And who are you visiting?\nYou: ")
               (talk-to-lady game 23))]
    [23 (match (safe-input)
          ["teo nistor" (begin (display "Lady: Good. I'll open the barriers on your right for you. Head through there and you will probably find him upstairs.\n")
                               (achieve-achievement (struct-copy game-state game [misc (dict-set (game-state-misc game) 'lady-status 2)][room-contents (room-add (game-state-room-contents game) 'office-right-2 "badge")]) 'creator))]
          [_ (begin (display "The lady fiddles on her computer for a while.\nLady: Sorry, it looks like no one with that name works here.\n")(talk-to-lady game 1))])]
    [32 (match (safe-input)
          ["teo nistor" (begin (display "Lady: Great! Here, I'll give you this temporary badge. Don't forget to return it at the end of the day.\n")
                               (achieve-achievement (struct-copy game-state game [misc (dict-set (game-state-misc game) 'lady-status 3)][inventory (set-add (game-state-inventory game) "badge")]) 'creator))]
          [_ (begin (display "The lady fiddles on her computer for a long time.\nLady: Sorry, I can't find your name in the system.\n")(talk-to-lady game 1))])])
  (begin
    (display "The lady doesn't pay you any attention.\n")
    game)))
          
(define (talk-to-cleaner game)
  (let ([updated-room-contents (room-remove (game-state-room-contents game) (game-state-location game) 'mess)])
    (if updated-room-contents
        (begin
          (display "You: Need some help there, pal?\nCleaner: I bloody well need some help! These bored salarymen threw a big one in here last night... that's all they ever did in this room since it was refurbished!\n")
          (if (set-member? (game-state-inventory game) "broom")
              (begin (display "You: Here, I'll give you a hand.\nYou use the broom you picked up to swiftly swipe the mess into bin bags.\n")
                     (achieve-achievement (struct-copy game-state game [room-contents updated-room-contents]) 'janitor))
              (begin (display "You: Well, I wish I could help you.\n")
                     game)))
        (begin
          (display "You open your mouth to say something, but no words come to mind.\n")
          game))))

(define (talk-to-barista game)
  (if (zero? (dict-ref (game-state-misc game) 'barista-status 0))
      (begin (display "Barista: Morning! What can I get for you today, sir?\nYou: A large coffee with no sugar, please.\nYou swipe your card and the barista places the large cup on the table.\n")
             (struct-copy game-state game [room-contents (room-add (game-state-room-contents game) 'cafe "coffee")][misc (dict-set (game-state-misc game) 'barista-status 1)]))
      (begin (display "The barista is busy cleaning the machines.\n")
             game)))

(define (talk game person)
  (match (list (game-state-location game) person)
    [(list 'reception "lady") (talk-to-lady game 0)]
    [(list 'office-left-1 "cleaner") (talk-to-cleaner game)]
    [(list 'cafe "barista") (talk-to-barista game)]
; TODO employee?

    [_ (begin (display "You open your mouth to say something, but no words come to mind.\n")
              game)]))

(define (inventory-loop inventory-list)
  (if (empty? inventory-list)
      (void)
      (begin (display ", ")
             (display (car inventory-list))
             (inventory-loop (cdr inventory-list)))))

(define (inventory game)
  (let ([inventory-list (game-state-inventory game)])
    (if (empty? inventory-list)
        (display " (nothing)")
        (begin (display (car inventory-list)) (inventory-loop (cdr inventory-list))))
    (newline))
  game)

(define (mission-accomplished game)
  (if (dict-ref (game-state-misc game) 'mission #f)
      game
      (begin (display " *** Mission accomplished ***\n *** You found the secret files\n *** Achievements unlocked: ")
             (display (length (dict-ref (game-state-misc game) 'achievements '())))
             (display "/3\n")
             (struct-copy game-state game [misc (dict-set (game-state-misc game) 'mission #t)]))))
     
(define (give game person object)
  (let* ([updated-inventory (inventory-remove (game-state-inventory game) object)]
         [updated-game (and updated-inventory
                            (match (list (game-state-location game) person object)
                              [(list 'office-left-1 "cleaner" "broom")
                                 (let ([updated-room-contents (room-remove (game-state-room-contents game) 'office-left-1 'mess)])
                                   (and updated-room-contents
                                        (begin (display "You: I think you'd be better of using this, pal.\nYou give the broom to the cleaner.\nCleaner: Jee, thanks!\n")
                                               (struct-copy game-state game [inventory updated-inventory] [room-contents updated-room-contents]))))]
                              [(list 'office-right-2 "employee" "coffee")(begin (display "You give the coffee to the tired employee.\nYou: You should go home if you're too tired.\nEmployee: Thanks, man! I really need to focus on this message spec.\nHe scratches his head, looking at some notes for a bit, then his head drops back on the desk.\n")
                                                                                (achieve-achievement (struct-copy game-state game [inventory updated-inventory]) 'coffee))]
                              [_ #f]))])
    (or updated-game
        (begin (display "You try to do that, but you fumble.\n")
               game))))


(define (bad-input game)
  (begin
    (display "You don't quite know how to do that.\n")
    game))

(define (check-exit)
  (begin
    (display "Really exit? (y/n) ")
    (let ([input (read-line)])
      (or (not (string? input)) (string=? "y" (string-downcase input))))))

(define (parse-input input)
  (match input
    [ "exit" #f]
    [ "look" look]
    [ "inventory" inventory]
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

(define (entry-point)

;[ "exit" #f]
 ;   [ "look" look]
  ;  [ "inventory" inventory]
   ; [(regexp #rx"^go( to| through| via|)( the)? (.+)$" (list _ _ _ where)) (λ (game) (go game where))]
;    [(regexp #rx"^(take|pick up)( the)? (.+)$" (list _ _ _ what))          (λ (game) (take game what))]
 ;   [(regexp #rx"^talk( to the| to|) (.+)$" (list _ _ who))                (λ (game) (talk game who))]
  ;  [(regexp #rx"^give( the)? (.+) to( the)? (.+)$" (list _ _ what _ who)) (λ (game) (give game who what))]
   ; [(regexp #rx"^give( the)? (.+)( the)? (.+)$" (list _ _ who _ what))    (λ (game) (give game who what))]

  
  (display "Commands to navigate the game:\n")
  (display " - look\n")
  (display " - inventory\n")
  (display " - go <forward/back/up/left/...>\n")
  (display " - take <object>\n")
  (display " - talk to <person>\n")
  (display " - give <object> to <person>\n")
  (display " - exit\n")
  (display " *** Your mission ***\n")
  (display "You have been sent to a competitor company's offices to uncover some secret files. You must infiltrate the building and find the files which are kept off-grid by the executive director.\n\n")

  (main-loop (look (game-state
                   
            ; inventory
            '()

            ; room contents
            (hash 'office-right-0 (set "banana")
                  'office-right-2 (set "notebook" "candy wrapper")
                  'office-left-1  (set "pencil" 'mess)
                  'office-left-2  (set "hard drive" "folder" "charger")
                  'broom-closet   (set "broom")
                  'restroom       (set "key"))

            ; starting location
            'car-park

            ; no special state to begin with
            (hash)))))

; (struct game-state (inventory room-contents room-descriptions room-doors location missions))

;(let ([gameee (mission-accomplished gamee)])
;  (display (game-state-misc gameee))
;  (game-state-inventory gameee))

;(inventory-contains gamee "a")

; TODO (define (entry-point)

(entry-point)