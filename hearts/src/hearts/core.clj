(ns hearts.core)

(def player-suits-broken "player pos then suit pos (y/n=1/0 for that suit) [[1 1 1 1] [0 1 1 0]]" (atom []))
(def suit-players-broken "suit pos then player pos (y/n = 1/0) [[0 0 0 0 0] [0 1 1 0 0]]" (atom []))
(def round-history "most recent hand first, most recent round first, sorted maps ordered by play, key is player, val is card '('({1 6 2 8 3 51 0 12}))" (atom '()))

(defn deal [game-state] "todo: expand beyond 1 deck, 4 players, add humans."
  (let [numplayers 4]
    (loop [players (vec (take (inc numplayers) (repeat (sorted-set))))
           cards (vec (take 52 (repeat 0)))
           loose-cards (range 52)
           needy-players (vec (partition 2 (interleave (range 1 (inc numplayers)) (repeat 0))))]
      (if (< (count loose-cards) 1)
        (if (> (count needy-players) 0)
          (throw (Exception. "some players still need cards"))
          (conj game-state
                {"player-cards" players ;"player/dealer pos assoc with cards in sorted-set (dealer is 0, other decks start at 52) [#{8 15 42 46 51} #{1 7 33 43}]. aces high"
                 "card-players" cards ;"card pos assoc with player/dealer num [1 4]"
                 "passed" [] ;player pos of this hand's passed cards [[4 '(0 51 40)] [1 '(37 38 12)]] ;source, cards
                 "card-history" (cons '() (game-state "card-history")) ;"hand pos then round pos (most recent to oldest), then player pos, '(([-1 0 -1 -1 12]))"
                 "first-player" (cons '() (game-state "first-player")) ;same structure as card-history to save order of play
                 "points-history" (cons [0 0 0 0 0] (game-state "points-history"))}))
        (let [chosen-index (rand-int (count needy-players))
              [chosen chosen-count] (needy-players chosen-index)]
          (recur (update players chosen conj (first loose-cards))
                 (update cards (first loose-cards) (constantly chosen))
                 (rest loose-cards)
                 (if (> chosen-count 11)
                   (vec (keep-indexed #(if (= chosen-index %) nil %2) needy-players))
                   (update needy-players chosen-index #(list (first %) (inc (second %)))))))))))

(defn game-start []
  (deal {"points-history" '() ;the accumulated points at the end of each hand, most recent first, then player pos '([0 13 4 0 9] [0 0 13 13 0])
         "card-history" '()
         "first-player" '()
         "pass-direction" -1 ;"-1=right, 1=left(clockwise, the order of the player-cards), 2=across(for now), 0=no passing"
         "human" [0 0 0 0 0]})) ;"player/dealer pos y/n [0 0 0 0 1]"

(defn move-card [card from to game-state]
  #_(prn "from" from "to" to "card" card)
  (when (= from to) (throw (Exception. "a player is passing to itself")))
  (conj game-state
        {"player-cards" (let [z (update (game-state "player-cards") from
                                        (fn [f] (if (contains? f card)
                                                  (disj f card)
                                                  (throw (Exception. "card not found")))))]
                          (update z to conj card))}
        {"card-players" (update (game-state "card-players") card (constantly to))}))

#_(defn human-pass [human-player target-player]
  (prn (str "Player " human-player ", what card of yours will you pass to player " target-player "?\n" "Your cards: " (@player-cards human-player)))
  (let [card (clojure.edn/read-string (read-line))] (if (= (type card) Long) (move-card card human-player target-player) (throw (Exception. "not a java.lang.Long")))))

(defn human-select-pass [player] nil)

(defn keep-queen [player score]
  (if (reduce #(or %1 (> %2 (+ 20 (score (- player 1)))))
              false
              score)
    1 90))

(defn pass [game-state]
  (update
   (if (= (game-state "pass-direction") 0)
     game-state
     (let [count-player-cards (count (game-state "player-cards"))]
       (loop [player (- count-player-cards 1)
              new-game-state game-state]
         (if (< player 1) new-game-state
           (recur (- player 1)
             (let [destination (condp = (+ player (game-state "pass-direction"));case doesn't work for some unknown reason
                                 count-player-cards 1
                                 0 (- count-player-cards 1)
                                 (inc count-player-cards) 2
                                 (+ player (game-state "pass-direction")))] ; edge cases
               #_(prn count-player-cards player (game-state "pass-direction") destination)
               (if (= ((game-state "human") player) 1)
                 (human-select-pass player)
                 (let [hand ((game-state "player-cards") player)
                       suit-nums (reduce #(update %1 (int (/ %2 13)) inc)
                                         [0 0 0 0]
                                         hand) ;num cards in each suit
                       suit-not-nums (mapv #(- 13 %) suit-nums) ; num cards not in each suit
                       score (first (game-state "points-history"))
                       special-card-weights {36 #_queen-spades (keep-queen player score)
                                             37 #_king-spades 100
                                             38 #_ace-spades 100
                                             0 #_two-clubs 18}
                       suit-weights [1 1 1 2] ;should keep low hearts
                       weights (map (fn [card] (vector (* (suit-weights (int (/ card 13)))
                                                          (special-card-weights card card) ;hand weight is the value of the card if no special weight
                                                          (suit-not-nums (int (/ card 13))))
                                                       card))
                                    hand)]
                   #_(prn weights)
                   (loop [top3 (take 3 (sort #(> (first %) (first %2)) weights)) ;[weight card]
                          new-game-state new-game-state]
                     (if (empty? top3) new-game-state
                       (recur (rest top3)
                         (move-card (second (first top3)) player destination new-game-state))))))))))))
     "pass-direction" #(condp = %, -1 1, 1 2, 2 0, 0 -1)))

;; makes low cards in suits that have been played/dealt more valuable ;in future can split into hand(player) and played(dealer) weights
;; just use 13 - card value for weights of broken suits, and 0.001 for suits that only I have
;;4 known cards=higher better, 5 known cards, lower=better
(defn card-suit-rarity [game-state]
  ((fn [hand]
     (mapv #(- (+ 14 (mod %2 13))
               (* 2 (+ 1 (((game-state "suits-known") (game-state "curr-player")) (int (/ %2 13)))) %))  ;more cards known --> lower more valuable ; maybe not good enough for encouraging throwing away isolated high cards
           (map #(/ (+ 1 (mod % 13)) 13) hand)  ;decrease by 1/13
           hand))
   ((game-state "player-cards2") (game-state "curr-player"))))

;;values to add to weights (highest will be chosen)
;throw only happens if I'm not able to win hand
;additional logic is required elsewhere for shooting the moon
(defn throw-weights [game-state]
  (let [curr-player (game-state "curr-player")]
    ((fn [hand]
       (let [points-total (first (game-state "points-history"))
             winning (game-state "winning")
             hand-points (apply mapv - (take 2 (concat (game-state "points-history") '([0 0 0 0 0]))))]
         (map #(cond ;no shoot the moon strategy for the queen of spades
                 (= % 36)
                 (if (or (and (not= (points-total curr-player) (apply min points-total)) ;not leading and
                              (> (+ 13 (points-total (first winning))) 99)) ; round winner about to end game
                         (and (> (- 30 (points-total curr-player)) (apply min points-total)) ;losing and
                              (< (- 13 (points-total curr-player)) (points-total (first winning))))) ;round winner is also losing
                   -1000 1000)
                 (or (= % 37) (= % 38))
                 (if (((game-state "player-cards") 0) 36) ;36 has been played
                   0 750)
                 (> % 38)
                 (+ % (cond (and (< 1 (count (filter zero? points-total))) ;potential shoot moon
                                 (not= (apply max hand-points) (hand-points curr-player)) ;I'm not shooting moon
                                 (not= (apply max hand-points) (hand-points (first winning)))) ;round winner is not shooting moon
                        500
                        (and (not= curr-player (first winning)) ;about to lose
                             (> (points-total (first winning)) 95))
                        -5
                        (or (= curr-player (first winning))
                            (< (points-total (first winning))
                               (+ 20 (points-total curr-player)))) ;don't kick a man while he's down, unless you're winning
                        2
                        :else 0))
                 :else 0)
              hand)))
     ((game-state "player-cards2") curr-player))))

(defn choice-init [game-state]
  (let [player-cards2 (mapv #(vec (sort %)) (game-state "player-cards"))
        player-suits (mapv (fn [hand] (reduce #(update %1 (int (/ %2 13)) inc)
                                              [0 0 0 0] hand))
                           player-cards2)]
    (conj game-state
          {"player-cards2" player-cards2  ;for mapping across multiple weights of constant pos
           "player-suits" player-suits
           "suits-known" (mapv #(mapv + % (player-suits 0)) player-suits)})))

;; throw smaller weight card if no valid one
;;returns card
(defn subsequent-choice [game-state]
  (let [card-suit-rarity (card-suit-rarity game-state)
        throw-weights (throw-weights game-state)
        suit (int (/ (second (game-state "winning")) 13))]
    (-> (fn [choice key weight]
          (let [card (((game-state "player-cards2") (game-state "curr-player")) key)]
            (cond (and (= (int (/ card 13)) suit)
                       (> weight (second choice)))  ;suit matches and more weight
              [card weight]
              (and (not= suit (int (/ (first choice) 13)))
                   (> (+ weight (throw-weights key)) (second choice)))  ;no matching suit yet and weight higher
              [card (+ weight (throw-weights key))]
              :else choice)))
        (reduce-kv [-1 0] (mapv * card-suit-rarity (game-state "forbidden")))  ;card, weight ;forbidden stops first round point throws
        first)))

(defn first-round-init [game-state]
  (let [fp ((game-state "card-players") 0)
        game-state (move-card 0 fp 0 game-state)]
    (conj game-state
          {"first-player" (cons (cons fp (first (game-state "first-player"))) (rest (game-state "first-player")))
           "card-history" (cons (cons [-1 -1 -1 -1 -1] (first (game-state "card-history"))) (rest (game-state "card-history")))
           "curr-player" (if (= fp 4) 1 (+ fp 1))
           "forbidden" (concat (take 36 (repeat 1)) '(0 1 1) (take 13 (repeat 0)))  ; only for first round-history
           "winning" [fp 0]}))) ;player, card

(defn subsequent-round-init [game-state]
  (let [fp (first (game-state "winning"))
        choice (choice (choice-init game-state))]
    (conj game-state
          {"first-player" (cons (cons fp (first (game-state "first-player"))) (rest (game-state "first-player")))
           "card-history" (cons (cons [-1 -1 -1 -1 -1] (first (game-state "card-history"))) (rest (game-state "card-history")))
           "curr-player" (if (= fp 4) 1 (+ fp 1))
           "winning" [fp choice]
           "forbidden" })))

(defn subsequent-play-card [game-state]
  (conj game-state
        {"curr-player" (if (= (game-state "curr-player") 4) 1 (inc (game-state "curr-player")))}))

#_(defn play-hand [game-state]
  ;; loop for each player
  (loop [game-state (hand-init-state game-state)]
    (if (> (count ((game-state "player-cards2") 0)) 51)
      game-state
      (let [choice (choice game-state)]

        (recur
          (if (= (game-state "first-player") curr-player)
            (first winning)
            (game-state "first-player"))
          (if (= (game-state "first-player") curr-player)
            (first winning)
            (let [np (+ curr-player 1)] (if (= np 4) 1 np)))
          (if (and (= suit (int (/ choice 13))) ; right suit
                   (> choice (second winning))) ; higher
            [curr-player choice]
            winning))))))



(defn -main
  [& args]
  (let [name (if (empty? args)
               (do (println "What is your name?") (read-line))
               (first args))]
    (println (str "hi " name))))
