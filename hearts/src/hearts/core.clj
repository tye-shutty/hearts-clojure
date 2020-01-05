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
                 "points-history" (cons [0 0 0 0 0] (game-state "points-history"))}))
        (let [chosen-index (rand-int (count needy-players))
              [chosen chosen-count] (needy-players chosen-index)]
          (recur (update players chosen conj (first loose-cards))
                 (update cards (first loose-cards) (constantly chosen))
                 (rest loose-cards)
                 (if (> chosen-count 11)
                   (vec (keep-indexed #(if (= chosen-index %) nil %2) needy-players))
                   (update needy-players chosen-index #(list (first %) (inc (second %)))))))))))

(defn start-game []
  (deal {"points-history" '([0 0 0 0 0]) ;the accumulated points at the end of each hand, most recent first, then player pos '([0 13 4 0 9] [0 0 13 13 0]) ;last element is all zeros
         "card-history" '() ;"hand pos then round pos (most recent to oldest), then player pos, '(([-1 0 -1 -1 12]))"
         "first-players" '()  ;same structure as card-history to save order of play
         "pass-direction" -1 ;"-1=right, 1=left(clockwise, the order of the player-cards), 2=across(for now), 0=no passing"
         "human" [0 0 0 0 0]})) ;"player/dealer pos y/n [0 0 0 0 1]"

(defn move-card [card from to game-state]
  #_(prn "from" from "to" to "card" card)
  (when (= from to) (throw (Exception. "a player is passing to itself")))
  (conj game-state
        {"player-cards" (let [z (update (game-state "player-cards") from
                                        (fn [f] (if (contains? f card)
                                                  (disj f card)
                                                  (throw (Exception. (str "card not found: " card))))))]
                          (update z to conj card))}
        {"card-players" (update (game-state "card-players") card (constantly to))}))

#_(defn human-pass [human-player target-player]
  (prn (str "Player " human-player ", what card of yours will you pass to player " target-player "?\n" "Your cards: " (@player-cards human-player)))
  (let [card (clojure.edn/read-string (read-line))] (if (= (type card) Long) (move-card card human-player target-player) (throw (Exception. "not a java.lang.Long")))))

(defn human-select-pass [player] nil)

(defn keep-queen [player score num-spades]
  (if (or (> num-spades 5) ;keep queen if have 6+ spades ;in future consider only low spades?
          (and (> num-spades 3)
               (reduce #(or %1 (> %2 (+ 20 (score (- player 1))))) ;keep queen if losing and have 4+ spades
                       false
                       score)))
    90 1))

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
               (if (= ((game-state "human") player) 1)
                 (human-select-pass player)
                 (let [hand ((game-state "player-cards") player)
                       suit-nums (reduce #(update %1 (quot %2 13) inc)
                                         [0 0 0 0]
                                         hand) ;num cards in each suit
                       suit-not-nums (mapv #(- 13 %) suit-nums) ; num cards not in each suit
                       score (first (game-state "points-history"))
                       special-card-weights {36 #_queen-spades (keep-queen player score (suit-nums 2))
                                             37 #_king-spades 100
                                             38 #_ace-spades 100
                                             0 #_two-clubs 18}
                       heart-weights (vec (concat (take 46 (repeat 1)) (take 6 (repeat 2)))) ;throw high hearts
                       weights (map (fn [card] (vector (* (heart-weights (quot card 13))
                                                          (special-card-weights card card) ;hand weight is the value of the card if no special weight
                                                          (suit-not-nums (quot card 13)))
                                                       card))
                                    hand)]
                   (loop [top3 (take 3 (sort #(> (first %) (first %2)) weights)) ;[weight card]
                          new-game-state new-game-state]
                     (if (empty? top3) new-game-state
                       (recur (rest top3)
                         (move-card (second (first top3)) player destination new-game-state))))))))))))
     "pass-direction" #(condp = %, -1 1, 1 2, 2 0, 0 -1)))

;; makes low cards in suits that have been played/dealt more valuable ;in future can split into hand(player) and played(dealer) weights
;;4 known cards=higher better, 5 known cards, lower=better
;; should only consider what is known at start of round
;;also need to consider winning card (only need to be lower)
(defn card-suit-rarity [game-state]
  ((fn [hand]
     (mapv #(- (+ 14 (mod %2 13))
               (* 2 (+ 1 (((game-state "suits-known") (game-state "curr-player")) (quot %2 13))) %))  ;more cards known --> lower more valuable ; maybe not good enough for encouraging throwing away isolated high cards
           (map #(/ (+ 1 (mod % 13)) 13) hand)  ;decrease by 1/13
           hand))
       (game-state "player-cards-sort")))

;;alternative weights (highest will be chosen)
;weight=0 means disallowed
;throw only happens if I'm not able to win hand
;additional logic is required elsewhere for shooting the moon

(defn throw-weights [game-state]
  (let [curr-player (game-state "curr-player")]
    ((fn [hand]
       (let [points-total (first (game-state "points-history"))
             winning (game-state "winning")
             hand-points (apply mapv - (take 2 (game-state "points-history")))]
         (map #(cond ;no shoot the moon strategy for the queen of spades
                 (= % 36)
                 (if (or (and (not= (points-total curr-player) (apply min points-total)) ;not leading and
                              (> (+ 13 (points-total (first winning))) 99)) ; round winner about to end game
                         (and (> (- 30 (points-total curr-player)) (apply min points-total)) ;losing and
                              (< (- 13 (points-total curr-player)) (points-total (first winning))))) ;round winner is also losing
                   0.0001 1000)
                 (or (= % 37) (= % 38))
                 (if (((game-state "player-cards") 0) 36) ;36 has been played
                   %2 750)
                 (> % 38)
                 (+ % (cond (and (< 1 (count (filter zero? points-total))) ;potential shoot moon
                                 (not= (apply max hand-points) (hand-points curr-player)) ;I'm not shooting moon
                                 (not= (apply max hand-points) (hand-points (first winning)))) ;round winner is not shooting moon
                        (+ 500 %2)
                        (and (not= curr-player (first winning)) ;about to lose
                             (> (points-total (first winning)) 95))
                        0.001
                        (or (= curr-player (first winning))
                            (< (points-total (first winning))
                               (+ 20 (points-total curr-player)))) ;don't kick a man while he's down, unless you're winning
                        (* 1.2 %2)
                        :else %2))
                 :else %2)
              hand
              (map (fn [card]
                     ;give more weight to suits broken by one person, little weight to suits broken by everyone else
                     (let [breakage (condp = (apply + ((game-state "suit-players-broken") (quot card 13)))
                                      0 1
                                      1 0.8
                                      2 0.7
                                      3 8)]
                       (/ (mod card 13)
                          ((game-state "player-suits") (quot card 13)) ;by default throw high cards from suits with few cards
                          breakage)))
                   hand))))
     (game-state "player-cards-sort"))))

;; throw smaller weight card if no valid one
;;returns card
(defn subsequent-choice [game-state]
  (let [player-cards-sort (vec (sort ((game-state "player-cards") (game-state "curr-player"))))
        game-state (conj game-state {"player-cards-sort" player-cards-sort})
        card-suit-rarity (card-suit-rarity game-state)
        throw-weights (throw-weights game-state)
        suit (quot (second (game-state "winning")) 13)]
    (let [legal-choice
          (-> (fn [choice key weight]
                (let [card (player-cards-sort key)
                      playable ((game-state "playable") card)]
                  (if (and (= (quot card 13) suit)
                             (not= 0 playable)
                             (> weight (second choice)))  ;suit matches and more weight
                    [card weight]
                    choice)))
              (reduce-kv [-1 0] card-suit-rarity)  ;card, weight ;playable stops first round point throws
              first)] )
    (if (= -1 legal-choice)
      (first (reduce-kv (fn [choice key weight]
                          (if (and (= 1 ((game-state "playable") (player-cards-sort key)))
                                   (> weight (second choice)))
                            [(player-cards-sort key) weight]
                            choice))
                        [-1 0]
                        throw-weights))
      legal-choice)))

;;returns card
(defn leading-choice [game-state]
  (let [card-suit-rarity (card-suit-rarity game-state)
        suit-count ((game-state "player-suits") (game-state "curr-player"))
        hand ((game-state "player-cards2") (game-state "curr-player"))]
    (->> (keep-indexed (fn [key weight]
                         (/ weight (suit-count (quot (hand key) 13)))))
         (reduce-kv (fn [acc key weight]
                      (if (and (> weight (second acc))
                               (not= 0 ((game-state "playable") (hand key))))
                        [(hand key) weight]
                        acc))
                    [-1 -1000]) ;card, weight
         ((fn [acc] (if (= -1 (first acc))
                      (first (reduce-kv (fn [acc key weight]
                                          (if (> weight (second acc))
                                            [(hand key) weight]
                                            acc))
                                        [-1 -1000]))
                      (first acc)))))))

(defn first-round-init [game-state]
  (let [fp ((game-state "card-players") 0)
        hist-init (update [-1 -1 -1 -1 -1] fp (constantly 0))
        player-cards-sort (mapv #(vec (sort %)) (game-state "player-cards"))
        player-suits (mapv (fn [hand] (reduce #(update %1 (quot %2 13) inc)
                                              [0 0 0 0] hand))
                           player-cards-sort)
        game-state (move-card 0 fp 0 game-state)]
    (conj game-state
          {"first-players" (cons (list fp) (game-state "first-players"))
           "card-history" (cons (list hist-init) (game-state "card-history"))
           "curr-player" (if (= fp 4) 1 (+ fp 1))
           "playable" (vec (concat (take 36 (repeat 1)) '(0 1 1) (take 13 (repeat 0))))  ; only for first round and when starting new rounds
           "winning" [fp 0]  ;player, card
           "player-suits" player-suits
           "suit-players-broken" (vec (take 4 (repeat (vec (take 5 (repeat 0))))));"suit pos then player pos (y/n = 1/0) [[0 0 0 0 0] [0 1 1 0 0]]"
           "suits-known" (mapv #(mapv + % (player-suits 0)) player-suits)}))) ;doubles dealer known (inconsequential)

(defn subsequent-round-init [game-state]
  (let [fp (first (game-state "winning"))
        game-state (conj game-state
                         {"playable" (vec (if (= 0 (apply + (apply map - (take 2 (game-state "points-history")))))
                                             (concat (take 36 (repeat 1)) '(0 1 1) (take 13 (repeat 0)))
                                             (take 52 (repeat 1))))})
        leading-choice (leading-choice game-state)
        hist-init (update [-1 -1 -1 -1 -1] fp (constantly leading-choice))]
    (conj (move-card leading-choice fp 0 game-state)
          {"first-players" (cons (cons fp (first (game-state "first-players")))
                                (rest (game-state "first-players")))
           "card-history" (cons (cons hist-init (first (game-state "card-history")))
                                (rest (game-state "card-history")))
           "curr-player" (if (= fp 4) 1 (+ fp 1))
           "winning" [fp leading-choice]
           "playable" (vec (take 52 (repeat 1)))})))

(defn subsequent-play-card [game-state]
  (let [choice (subsequent-choice game-state)]
    (conj (move-card choice (game-state "curr-player") 0 game-state)
          {"curr-player" (if (= (game-state "curr-player") 4)
                           1 (+ (game-state "curr-player") 1))
           "winning" (if (and (> choice (second (game-state "winning")))
                              (= (quot choice 13) (quot (second (game-state "winning")) 13)))
                       [(game-state "curr-player") choice]
                       (game-state "winning"))
           "card-history" (cons (cons (update (first (first (game-state "card-history")))
                                              (game-state "curr-player")
                                              (constantly choice))
                                      (rest (first (game-state "card-history"))))
                                (rest (game-state "card-history")))
           "suit-players-broken" (let [suit (quot (second (game-state "winning")) 13)]
                                   (if (= suit (quot choice 13))
                                     (game-state "suit-players-broken")
                                     (update (game-state "suit-players-broken")
                                             suit
                                             (fn [players]
                                               (update players
                                                       (game-state "curr-player")
                                                       (constantly 1))))))})))

#_(defn play-hand [game-state]
  ;; loop for each player
  (loop [game-state (hand-init-state game-state)]
    (if (> (count ((game-state "player-cards2") 0)) 51)
      game-state
      (let [choice (choice game-state)]

        (recur
          (if (= (game-state "first-players") curr-player)
            (first winning)
            (game-state "first-players"))
          (if (= (game-state "first-players") curr-player)
            (first winning)
            (let [np (+ curr-player 1)] (if (= np 4) 1 np)))
          (if (and (= suit (quot choice 13)) ; right suit
                   (> choice (second winning))) ; higher
            [curr-player choice]
            winning))))))



(defn -main
  [& args]
  (let [name (if (empty? args)
               (do (println "What is your name?") (read-line))
               (first args))]
    (println (str "hi " name))))
