(ns hearts.core)

;;most of this can be put in a map and passed between functions, but atoms are good for troubleshooting and piecemeal development
(def player-cards "player/dealer pos assoc with cards in sorted-set (dealer is 0, other decks start at 52) [#{8 15 42 46 51} #{1 7 33 43}]. aces high" (atom []))
(def card-players "card pos assoc with player/dealer num [1 4]" (atom []))
(def player-suits-broken "player pos then suit pos (y/n=1/0 for that suit) [[1 1 1 1] [0 1 1 0]]" (atom []))
(def suit-players-broken "suit pos then player pos (y/n = 1/0) [[0 0 0 0] [0 1 1 0]]" (atom []))
(def curr-winner "player and card" (atom [-1 -1]))
(def hand-points "player pos [0 0 0 1]" (atom []))
(def round-history "most recent hand first, most recent round first, sorted maps ordered by play, key is player, val is card '('({1 6 2 8 3 51 0 12}))" (atom '()))
(def player-history "no purpose yet, doesn't have play order, just ordered by player then by round [[9 22] [5 16] [12 47] [0 13]]")

(defn deal [] "todo: expand beyond 1 deck, 4 players, add humans."
  (let [numplayers 4]
    (loop [players (vec (take (inc numplayers) (repeat (sorted-set))))
           cards (vec (take 52 (repeat 0)))
           loose-cards (range 52)
           needy-players (vec (partition 2 (interleave (range 1 (inc numplayers)) (repeat 0))))]
      (if (< (count loose-cards) 1)
        (if (> (count needy-players) 0)
          (throw (Exception. "some players still need cards"))
          {"player-cards" players
           "card-players" cards
           "pass-direction" -1 ;"-1=right, 1=left(clockwise, the order of the player-cards), 2=across(for now), 0=no passing"
	   "points-history" '([0 0 0 0 0]) ;"hand pos (most recent to oldest) then player pos '([0 13 4 0 9] [0 0 13 13 0])"
	   "points-total" [0 0 0 0 0]
           "passed" [] ;player pos of most recent given cards
           "human" [0 0 0 0 0]}) ;"player/dealer pos y/n [0 0 0 0 1]"
        (let [chosen-index (rand-int (count needy-players))
              [chosen chosen-count] (needy-players chosen-index)]
          (recur (update players chosen conj (first loose-cards))
            (update cards (first loose-cards) (constantly chosen))
            (rest loose-cards)
            (if (> chosen-count 11)
              (vec (keep-indexed #(if (= chosen-index %) nil %2) needy-players))
              (update needy-players chosen-index #(list (first %) (inc (second %)))))))))))

(defn move-card [card from to game-state]
  (prn "from" from "to" to "card" card)
  (when (= from to) (throw (Exception. "a player is passing to itself")))
  (conj game-state
        {"player-cards" (let [z (update (game-state "player-cards") from
                                        (fn [f] (if (contains? f card)
                                                  (disj f card)
                                                  (throw (Exception. "card not found")))))]
                          (update z to conj card))}
        {"card-players" (update (game-state "card-players") card (constantly to))}))

(defn human-pass [human-player target-player]
  (prn (str "Player " human-player ", what card of yours will you pass to player " target-player "?\n" "Your cards: " (@player-cards human-player)))
  (let [card (clojure.edn/read-string (read-line))] (if (= (type card) Long) (move-card card human-player target-player) (throw (Exception. "not a java.lang.Long")))))

(defn human-select-pass [player] nil)

(defn keep-queen [player score]
  (if (reduce #(or %1 (> %2 (+ 20 (score (- player 1)))))
              false
              score)
    1 90))

(defn pass [game-state]
  (if (= (game-state "pass-direction") 0) nil
    (update
     (let [count-player-cards (count (game-state "player-cards"))]
       (loop [player (- count-player-cards 1)
              game-state game-state]
         (if (< player 1) game-state
           (recur (- player 1)
             (let [destination (case (+ player (game-state "pass-direction"))
                                   count-player-cards 1
                                   0 (- count-player-cards 1)
                                   (+ player (game-state "pass-direction")))] ; edge cases
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
                               game-state game-state]
                          (if (empty? top3) game-state
                            (recur (rest top3)
                                   (move-card (second (first top3)) player destination game-state)))))))))))
     "pass-direction" #(case %, -1 1, 1 2, 2 0, 0 -1))))

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
;throw only happens if I'm not winning
(defn throw-weights [game-state]
  (let [curr-player (game-state "curr-player")]
    ((fn [hand]
       (let [points-total (game-state "points-total")
             winning (game-state "winning")
             points-history (game-state "points-history")]
         (map #(cond ;no shoot the moon strategy for the queen of spades
                 (= % 36)
                 (if (or (and (not= (points-total curr-player) (apply min points-total)) ;not leading and
                              (> (+ 13 (points-total (first winning))) 99)) ; round winner about to end game
                         (and (> (- 30 (points-total curr-player)) (apply min points-total)) ;losing and
                              (< (- 13 (points-total curr-player)) (points-total (first winning))))) ;round winner is also losing
                   -1000 1000)
                 (or (= % 37) (= % 38))
                 (if (reduce (fn [a v] (or a (= 36 (second v))))
                             false
                             (take-last (mod (count points-history) 4)))
                   0 750)
                 (> % 38)
                 (+ % (cond (and (= 3 (count (filter zero? (first points-history)))) ;potential shoot moon
                                 (not= (max (first points-history)) curr-player) ;I'm not shooting moon
                                 (not= (max (first points-history)) ((first points-history) (first winning)))) ;round winner is not shooting moon
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

(defn hand-init-state [game-state]
  (let [fp ((game-state "card-players") 0)
        game-state (move-card 0 fp 0 game-state)
        player-cards2 (mapv #(vec (sort %)) (game-state "player-cards"))
        player-suits (mapv (fn [hand] (reduce #(update %1 (int (/ %2 13)) inc)
                                              [0 0 0 0] hand))
                           player-cards2)]
    (conj game-state
          {"first-player" fp
           "curr-player" (let [np (+ fp 1)] (if (= np 5) 1 np))
           "winning" [fp 0]  ; player, card
           "hand-history" (list [fp 0])  ;newest at beginning
           "broken" (concat (take 36 (repeat 1)) '(0 1 1) (take 13 (repeat 0)))  ; to be multiplied by weights
           "player-cards2" player-cards2  ;for mapping across multiple weights of constant pos
           "player-suits" player-suits
           "suits-known" (mapv #(mapv + % (player-suits 0)) player-suits)})))

;; throw smaller weight card if no valid one
(defn choice [game-state]
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
        (reduce-kv [-1 0] (mapv * card-suit-rarity (game-state "broken")))  ;card, weight ;broken stops first round point throws
        first)))

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
