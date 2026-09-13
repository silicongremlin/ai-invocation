#lang racket
(require racket/list)
(require racket/string)

; Define the structures for our data
(struct item (name visible? location found? value) #:mutable)
(struct location (name description items next-moves prompt))
(struct game-state (current-location inventory player-name))

; Initialize inventory
(define (inventory)
  (list))  ; Start with an empty list

; Initialize available items
(define (available-items)
  (list (item 'boots #false 'coastline #false 0)
        (item 'helmet #false 'cave #false 0)
        (item 'gold #false 'shipwreck #false 10)
        (item 'rope #false 'cliffside #false 0))) ; TODO during summer: Add a +2 for the cliffside roll only if rope is present in inventory.
 
; helper function to dind an item by its name in a list of items
(define (find-item-by-name name items)
  (cond [(empty? items) #false]
        [(equal? name (item-name (first items))) (first items)]
        [else (find-item-by-name name (rest items))]))

; Helper function to find item by name in the inventory 
(define (get-item-name item)
  (item-name item))

; Helper function to get item name if it exisits in the inventory
(define (get-item-name-if-exists target-item-name inventory)
  (let ((names (map get-item-name inventory)))
    (if (member target-item-name names)
        target-item-name
        #f)))

; Function to update inventory items
(define (update-inventory item inventory)
  (if (not (find-item-by-name (item-name item) inventory))
      (cons item inventory)  ; Add the item to the inventory if not found
      inventory))           ; Otherwise, return the existing inventory

; Function to remove an item from inventory when dropped
(define (remove-item-from-inventory target-item-name state)
  (let ((inventory (game-state-inventory state)))
    (display (format "Current inventory before dropping: ~a\n" (map get-item-name inventory))) 
    (let ((target-symbol (string->symbol target-item-name)))  ; Convert target item name to symbol
      (let ((new-inventory (filter (lambda (i) (not (equal? (item-name i) target-symbol))) inventory)))
        (display (format "New inventory after dropping: ~a\n" (map get-item-name new-inventory))) 
        (game-state (game-state-current-location state) new-inventory (game-state-player-name state))))))  ; Return updated state

; Function to Display inventory
(define (print-inventory inventory)
  (if (null? inventory)
      (display "You currently have no items.\n")
      (begin
        (display "You currently have the following items:\n")
        (for-each (lambda (item)
                    (display (format "- ~a\n" (item-name item))))
                  inventory))))

; Definintions for all of our locations
(define coastline (location 'coastline 
                            "As you slowly wake up, you find yourself washed up on a shore...\nYou notice a tattered bag in the distance down the coast...\n" 
                            (inventory) 
                            '(shipwreck cave cliffside)
                            "Do you want to head West to search it? (West/No)\n\n"))
(define shipwreck (location 'shipwreck 
                            "You notice your ship wrecked up the coastline.\nThere is no sign of life as far as the eye can see..."
                            (inventory) 
                            '(coastline cave cliffside)
                            "Would you like to travel South to look for any survivors? (South/No)\n"))
(define cave (location 'cave 
                       "There is an eerie cave a little bit further east.\nThere might be something valuable hidden here..." 
                       (inventory)  
                       '(coastline shipwreck cliffside)
                       "Would you like to explore the cave? (East/No)\n"))
(define cliffside (location 'cliffside 
                            "Ahead is a cliff...\nThis vantage point may be high enough up to search for help...\n"
                            (inventory) 
                            '(coastline shipwreck cave)
                            "Would you like to head North to explore? (North/No)\n"))

; Helper function to that will check if the location name matches a name in our defined locations above
; If it is true, return the location name else throw error to user.
(define (get-location location-name)
  (cond [(eq? location-name 'coastline) coastline]
        [(eq? location-name 'shipwreck) shipwreck]
        [(eq? location-name 'cave) cave]
        [(eq? location-name 'cliffside) cliffside]
        [else (error "Unknown location")]))

; Initialize game state
(define initial-state (game-state 'coastline (inventory) "PlayerNameHere"))

; Function to downcase all user input
(define (downcase-input input)
  (string-trim (string-downcase input)))

; Dice roll functions
(define (d6-roll) (+ (random 6) 1))
(define (d8-roll) (+ (random 8) 1))

; Function to prompt user for the next location.
(define (next-location-prompt state)
  (let* ((loc (get-location (game-state-current-location state)))
         (next-moves (location-next-moves loc)))
    (display "Where would you like to go next? Your options are:\n\n")
    (for-each (lambda (dir)
                (display (format "~a\n" dir)))
              next-moves)
    (display "\nChoose a location to travel to next (or type 'stay' to remain here):\n")
    (let ((input (downcase-input (read-line))))
      (cond
        [(member (string->symbol input) next-moves) (change-location (string->symbol input) state)]
        [(string=? input "stay") (game-loop state)]  ; Allow the player to choose to stay, this was difficult to get working
        [else (display "Invalid input, please choose a valid location.\n")
              (next-location-prompt state)]))))

; Helper function to change locations and update state
(define (change-location new-loc state)
  (let ((new-state (game-state new-loc (game-state-inventory state) (game-state-player-name state))))
    (game-loop new-state)))

; Function to handle logic for commands
(define (await-command state)
  (display "Type help for a list of commands, enter a command, or follow the directional prompts above: ") ; Prompt user for their input
  (let ((input (read-line)))
    (let ((command (first (string-split (string-trim (string-downcase input)))))
          (args (rest (string-split (string-trim (string-downcase input))))))
      (case command
        [("west")
         (handle-west state)]
        [("north")
         (handle-north state)]
        [("south")
         (handle-south state)]
        [("east")
         (handle-east state)]
        [("inv")
         (print-inventory (game-state-inventory state))
         (await-command state)]
        [("drop")
         (if (null? args)
             (begin
               (display "You need to specify an item to drop.\n")
               (await-command state))
             (let ((item-name (string-join args " "))) ; Concatenate args for multi-word item names
               (let ((new-state (remove-item-from-inventory item-name state)))
                 (await-command new-state))))]
        [("no")
         (next-location-prompt state)]
        [("help")
         (display-commands)
         (await-command state)]
        [("proceed")
         (game-loop state)]
        [else
         (display "Invalid command, try again.\n")
         (await-command state)]))))

; Function to display to display the different commands to the user
(define (display-commands)
  (display "Available commands:\n")
  (display "- 'inv': Show items in your inventory.\n")
  (display "- 'drop [item]': Remove an item from your inventory.\n")
  (display "- 'help': Show this commands menu.\n")
  (display "- 'proceed': Resume your adventure.\n")
  (display "Type 'proceed' or any other command to proceed with your adventure.\n"))

; Function to start the game by asking player's name
(define (start-game)
  (display "What's your name, adventurer?\n")
  (let ((name (read-line)))
    (let ((state (game-state 'coastline (inventory) name))) ; Create initial state with player name
      (display (format "Welcome, ~a!\n\n" name))
      (display "Type 'help' to see the list of commands.\n\n")
      (game-loop state))))

; Main game loop
(define (game-loop state)
  (let* ((loc (get-location (game-state-current-location state)))
         (desc (location-description loc))
         (prompt (location-prompt loc)))
    (display desc) ; Display location description and prompt only initially or on specific continuation
    (display "\n")
    (display prompt)
    (await-command state)))

; Logic for heading West towards the coastline.
; Item 1
(define (handle-west state)
  (display "You search the bag...\n")
  (let ((roll (d6-roll)))
    (display (format "Rolling the dice... You rolled a ~a!\n" roll))
    (if (>= roll 3)
        (begin
          (display "In the bag you find a tattered pair of boots!\n")
          (let ((found-item (find-item-by-name 'boots (available-items))))
            (let ((new-inventory (update-inventory found-item (game-state-inventory state))))
              (display "At least the rest of the journey won't be as rugged...\n")
              (let ((new-state (game-state (game-state-current-location state) new-inventory (game-state-player-name state))))
                (next-location-prompt new-state)))))  ; Prompt for next location
        (begin
          (display (format "You found nothing.\n" roll))
          (next-location-prompt state)))))  ; Continue from the current state without finding anything

; Logic for heading south towards the shipwreck
; Item 2
(define (handle-south state)
  (display "As you head towards the ship...\n")
  (display "There are no signs of life...\nHowever, in the wreckage, you see a safe that looks to be open.\n")
  (display "Do you want to investigate this? (Yes/No)\n")
  (let ((input (downcase-input (read-line))))
    (cond
      ((string=? input "yes")
       (let ((roll (d6-roll)))
         (display (format "Rolling the dice... You rolled a ~a!\n" roll))
         (if (>= roll 3)
             (begin
               (display "You search the safe and find some gold.\n")
               (let ((found-item (find-item-by-name 'gold (available-items))))  ; Use (inventory) to get the static definition
                 (if found-item
                     (let ((new-inventory (update-inventory found-item (game-state-inventory state))))
                       (display "You place the gold in your pockets...\n")
                       (let ((new-state (game-state (game-state-current-location state) new-inventory (game-state-player-name state))))

                         (next-location-prompt new-state)))
                     (begin
                       (display "Error: Item not found in inventory.\n")
                       (next-location-prompt state)))))
             (begin
               (display "You found nothing of interest.\n")
               (next-location-prompt state)))))
      ((string=? input "no")
       (begin
         (display "You decide not to investigate the safe.\n")
         (next-location-prompt state)))
      (else
       (begin
         (display "Invalid input, please choose 'yes' or 'no'.\n")
         (handle-south state))))))

; Logic for heading East toward the Cave
; Item 3
(define (handle-east state)
  (display "As you enter the cave, there is a strange feeling that someone is watching you...\n")
  (display "You notice a glint further in the cave...\n")
  (display "As you get closer, you make out a helmet still attached to the remains of a previous traveler.\n")
  (display "Are you brave enough to search the remains? (Yes/No)\n")
  (let ((input (downcase-input (read-line))))
    (cond
      ((string=? input "yes")
       (let ((roll (d6-roll)))
         (display (format "Rolling the dice... You rolled a ~a!\n" roll))
         (if (>= roll 3)
             (begin
               (display "Searching the remains, you find a helmet!\n")
               (let ((found-item (find-item-by-name 'helmet (available-items))))
                 (let ((new-inventory (update-inventory found-item (game-state-inventory state))))
                   (display "You think to yourself, this may come in handy later...\n")
                   (let ((new-state (game-state (game-state-current-location state) new-inventory (game-state-player-name state))))

                         (next-location-prompt new-state)))))
             (begin
               (display (format "You found nothing of interest.\n" roll))
               (next-location-prompt state)))))
      ((string=? input "no")
       (display "You decide not to search the remains.\n")
       (next-location-prompt state))
      (else
       (begin
         (display "Invalid input, please choose 'yes' or 'no'.\n")
         (handle-east state))))))

; Logic for heading North toward the cliffside
; Item 4
(define (handle-north state)
  (display "During your walk towards the cliff, you notice a rope on the ground.\nWould you like to pick this up? (Yes/No)\n")
  (let ((rope-input (downcase-input (read-line))))
    (cond
      [(string=? rope-input "yes") 
       (display "You pick up the rope with hopes it will make this climb easier.\n")
       (let ((found-item (find-item-by-name 'rope (available-items)))) ; Get the static rope item definition
         (if found-item
             (let ((new-inventory (update-inventory found-item (game-state-inventory state))))
               (let ((new-state (game-state (game-state-current-location state) new-inventory (game-state-player-name state))))
                 (continue-cliffside new-state)))
             (begin
               (display "Error: Item not found in inventory.\n")
               (continue-cliffside state))))]  ; Handle error if rope is not found
      [(string=? rope-input "no")
       (display "You decide not to take the rope.\n")
       (continue-cliffside state)]  ; Continue with the current state
      [else
       (display "Invalid input, try again:\n")
       (continue-cliffside state)])))  ; Reprompt if invalid input

; Helper function for cliffside to handle location logic
(define (continue-cliffside state)
  (display "\nSome time later...\n")
  (display "As you reach the cliff, it's time for you to decide if it is worth the risk.\n")
  (display "\nWARNING: FAILURE TO PASS THIS DICE ROLL COULD RESULT IN DEATH.\n\n")
  (display "Do you wish to continue? (Yes/No)\n")
  (let ((explore-input (downcase-input (read-line))))
    (cond
      [(string=? explore-input "yes")
       (let ((roll (d8-roll)))
         (cond
           [(>= roll 7)
            (display (format "You successfully climb the cliff and see a search party!\nYou rolled a ~a to pass the dexterity check!\n\nGAME OVER.\n\n" roll))
            (display "Would you like to play again? (Yes/No)\n")
            (when (string=? (downcase-input (read-line)) "yes")
              (game-loop initial-state))]  ; This will allow the option to return to the initial game start. "Fresh start" 
           [(and (> roll 3) (<= roll 6))
            (display (format "You fall but a bush breaks your fall... You rolled a ~a!\n\n\nGAME OVER.\n\n" roll))
            (display "Would you like to play again? (Yes/No)\n")
            (when (string=? (downcase-input (read-line)) "yes")
              (game-loop initial-state))] 
           [else
            (display (format "Your low dexterity causes you to slip and fall to your death.\nYou rolled a ~a! 4+ is required to survive.\n\nGAME OVER.\n\n" roll))
            (display "Would you like to play again? (Yes/No)\n")
            (when (string=? (downcase-input (read-line)) "yes")
              (game-loop initial-state))])   
        (next-location-prompt state))]
      [(string=? explore-input "no")
       (display "You decide not to risk climbing the cliff right now.\n")
       (next-location-prompt state)] 
      [else
       (display "Invalid input, try again:\n")
       (continue-cliffside state)])))  ; Handle invalid inputs within continue-cliffside

; Start the game
(start-game)
