(ns hearts.core
  (:gen-class)
  (:require [hearts.on-suit-weights :refer [on-suit-weights]]
            [hearts.pass-ns :refer [pass]]
            [hearts.common-helpers :refer [move-card]]
            [hearts.off-suit-weights-ns :refer [off-suit-weights]]))
;(use ['hearts.core-test :refer :all])
#_(def player-suits-broken "player pos then suit pos (y/n=1/0 for that suit) [[1 1 1 1] [0 1 1 0]]" (atom []))

(defn create-game []
  {;;the cumulative points at the end of each hand, most recent first,
         ;then player pos '([0 13 4 0 9] [0 0 13 13 0]) ;last element is all
         ;zeros
   "turn-depth->(player->cumulative-points)" '([0 0 0 0 0])
         ;;"hand pos then round pos (most recent to oldest), then player pos,
         ;'(([-1 0 -1 -1 12]))"
   "hand->round->player->card" '()
         ;;same structure as hand->round->player->card to save order of play
   "hand->round->player->play-order" '()
         ;;"-1=right, 1=left(clockwise, the order of the player->card-set),
         ;2=across(for now), 0=no passing"
   "pass-direction" -1
   "shoot-moon" [false false false false false]
         ;;"player/dealer pos y/n [0 0 0 0 1]"
   "human" [0 0 0 0 0]})

(defn deal
  "todo: expand beyond 1 deck, 4 players, add humans."
  [game-state]
  (let [numplayers 4]
    (loop [players (vec (take (inc numplayers) (repeat (sorted-set))))
           cards (vec (take 52 (repeat 0)))
           loose-cards (range 52)
           needy-players (vec (partition 2 (interleave (range 1 (inc numplayers)) (repeat 0))))]
      (if (< (count loose-cards) 1)
        (if (> (count needy-players) 0)
          (throw (Exception. "some players still need cards"))
          (conj game-state
                {"player->card-set" players ;"player/dealer pos assoc with cards in sorted-set (dealer is 0, other decks start at 52) 
                ;;  [#{8 15 42 46 51} #{1 7 33 43}]. aces high"
                 "card->player" cards ;"card pos assoc with player/dealer num [1 4]"
                 "passer->passee" {[0 0] '()} ;player pos of this hand's passed cards {[1 4] '(0 51 40) [4 1] '(37 38 12)} 
                 ;{[source dest] '(cards)}
                 "turn-depth->(player->cumulative-points)" (cons [0 0 0 0 0] (game-state "turn-depth->(player->cumulative-points)"))}))
        (let [chosen-index (rand-int (count needy-players))
              [chosen chosen-count] (needy-players chosen-index)]
          (recur (update players chosen conj (first loose-cards))
                 (update cards (first loose-cards) (constantly chosen))
                 (rest loose-cards)
                 (if (> chosen-count 11)
                   (vec (keep-indexed #(if (= chosen-index %) nil %2) needy-players))
                   (update needy-players chosen-index #(list (first %) (inc (second %)))))))))))

(defn passer->passee+cards
  [pass-history passer]
  (let [history (filter #(= [passer (second (first %))] (first %)) pass-history)]
    [(second (first history)) (second history)]))

(defn init-36-probs
  [pass-direction card->player passer->passee passer->cards]
  (vec (keep-indexed
        (fn [player _]
          (if (= -1 pass-direction)
            (if (= player (card->player 36))
              (update [0 0 0 0 0] player (constantly 1))
              (update [0 0.33 0.33 0.33 0.33] player (constantly 0)))
            (let [passee (passer->passee player)
                  passee-prob (if (= player (passer->passee passee))
                                0.18
                                0.26)]
              (if (some #{36} (passer->cards player))
                (update [0 0 0 0 0] passee (constantly 1))
                (if (= player (card->player 36))
                  (update [0 0 0 0 0] player (constantly 1))
                  (as-> (vec (cons 0 (take 4 (repeat (/ (- 1 passee-prob) 2))))) probabilities
                    (update probabilities player (constantly 0))
                    (update probabilities passee (constantly passee-prob))))))))
        (vec (take 5 (repeat [0 0 0 0 0]))))))

(defn organize-new-hand
  [game-state]
  (let [player->cards (mapv #(vec (sort %)) (game-state "player->card-set"))
        player->suit->count (mapv (fn [hand] (reduce #(update %1 (quot %2 13) inc)
                                                       [0 0 0 0] hand))
                                    player->cards)]
    (conj game-state
          {"hand->round->player->play-order" (cons (list) (game-state "hand->round->player->play-order"))
           "hand->round->player->card" (cons (list [-2 -1 -1 -1 -1]) (game-state "hand->round->player->card"))
           "playable" (vec (concat (take 36 (repeat 1)) '(0 1 1) (take 13 (repeat 0))))
           "player->suit->count" player->suit->count
           "suit->player->broken" (vec (take 4 (repeat (vec (take 5 (repeat 0))))))
           ;played off suit in that suit (y/n = 1/0)
           "player->player->could-have-36" (init-36-probs (game-state "pass-direction")
                                                          (game-state "card->player")
                                                          (game-state "passer->passee")
                                                          (game-state "passer->cards"))
           ;probabiliy players that might have queen of spades, considers that they'd
           ;play it asap also consider gameending implications of playing as a
           ;reason why they might retain it.
           "suits-known" (mapv #(mapv + % (player->suit->count 0)) player->suit->count)
           ;suits known to have been played or in hand
           })))

(defn organize-new-round
  [game-state]
  (let [curr-player (if (not= 0 ((game-state "card->player") 0))
                      ((game-state "card->player") 0)
                      (game-state "winning-player"))
        player->order (loop [c [-1 0 0 0 0]
                             order (take 4 (iterate #(if (= 4 %) 1 (+ 1 %)) curr-player))
                             i 0]
                        (if (= i 4) c
                            (recur (update c (order i) (constantly i)) order (inc i))))
        game-state (dissoc game-state "winning-player" "winning-card")]
    (conj game-state {"curr-player" curr-player
                      "hand->round->player->play-order"
                      (cons (cons player->order (first (game-state "hand->round->player->play-order")))
                            (rest (game-state "hand->round->player->play-order")))})))

(defn subsequent-choice
  "Returns best card to play when not leading choice"
  [game-state]
  (let [curr-hand (vec (sort ((game-state "player->card-set")
                              (game-state "curr-player"))))
        game-state (conj game-state {"curr-hand" curr-hand})
        suit (quot (second (game-state "winning")) 13)
        weights (if (= 0 (((game-state "player->suit->count")
                           (game-state "curr-player"))
                          suit))
                  (off-suit-weights game-state)
                  (on-suit-weights game-state))]
    (.indexOf weights (apply max weights))))  ;returns first match (low bias)

;;returns card
#_(defn leading-choice [game-state]
    (let [on-suit-weight (on-suit-weight game-state)
          suit-count ((game-state "player->suit->count")
                      (game-state "curr-player"))
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

(defn play-card
  [game-state]
  (let [choice (if (not= 0 ((game-state "card->player") 0))
                 0
                 (subsequent-choice game-state))
        game-state (move-card choice (game-state "curr-player") 0 game-state)
        curr-winning? (or (not (contains? game-state "winning"))
                          (and (> choice (second (game-state "winning")))
                               (= (quot choice 13) (quot (second (game-state "winning")) 13))))]
    (conj game-state
          {"curr-player" (if (= (game-state "curr-player") 4)
                           1 
                           (+ (game-state "curr-player") 1))
           "winning-player" (if curr-winning?
                              (game-state "curr-player")
                              (game-state "winning-player"))
           "winning-card" (if curr-winning?
                              choice
                              (game-state "winning-card"))
           "hand->round->player->card" (cons (cons (update (first (first (game-state "hand->round->player->card")))
                                                           (game-state "curr-player")
                                                           (constantly choice))
                                                   (rest (first (game-state "hand->round->player->card"))))
                                             (rest (game-state "hand->round->player->card")))
           "suit->player->broken" (let [suit (quot (second (game-state "winning")) 13)]
                                   (if (= suit (quot choice 13))
                                     (game-state "suit->player->broken")
                                     (update (game-state "suit->player->broken")
                                             suit
                                             (fn [players]
                                               (update players
                                                       (game-state "curr-player")
                                                       (constantly 1))))))})))

#_(defn subsequent-round-init [game-state]
    (let [fp (first (game-state "winning"))
          game-state (conj game-state
                           {"playable" (vec (if (= 0 (apply + (apply map - (take 2 (game-state "turn-depth->(player->cumulative-points)")))))
                                              (concat (take 36 (repeat 1)) '(0 1 1) (take 13 (repeat 0)))
                                              (take 52 (repeat 1))))})
          leading-choice (leading-choice game-state)
          hist-init (update [-1 -1 -1 -1 -1] fp (constantly leading-choice))]
      (conj (move-card leading-choice fp 0 game-state)
            {"hand->round->player->play-order" (cons (cons fp (first (game-state "hand->round->player->play-order")))
                                                (rest (game-state "hand->round->player->play-order")))
             "hand->round->player->card" (cons (cons hist-init (first (game-state "hand->round->player->card")))
                                               (rest (game-state "hand->round->player->card")))
             "curr-player" (if (= fp 4) 1 (+ fp 1))
             "winning" [fp leading-choice]
             "playable" (vec (take 52 (repeat 1)))})))

#_(defn play-hand [game-state]
  ;; loop for each player
    (loop [game-state (hand-init-state game-state)]
      (if (> (count ((game-state "player-cards2") 0)) 51)
        game-state
        (let [choice (choice game-state)]

          (recur
           (if (= (game-state "hand->round->player->play-order") curr-player)
             (first winning)
             (game-state "hand->round->player->play-order"))
           (if (= (game-state "hand->round->player->play-order") curr-player)
             (first winning)
             (let [np (+ curr-player 1)] (if (= np 4) 1 np)))
           (if (and (= suit (quot choice 13)) ; right suit
                    (> choice (second winning))) ; higher
             [curr-player choice]
             winning))))))

(defn game-over?
  [state]
  (some #(> % 100) ((state "turn-depth->(player->cumulative-points)") 0)))

(defn -main
  []
  ;play-card could be ML, explicit AI takes entire game into account ;has if statement to check if first round
  (loop [state (create-game)]
    (if (game-over? state)
      state
      (-> state
          (deal)
          (pass)
          (organize-new-hand) ;doesn't include playing 2 of clubs
          (#(loop [state %
                   i 0]
              (if (= i 12)
                state
                (recur (loop [state (organize-new-round state)
                              j 0]
                         (if (= j 4)
                           state
                           (recur (play-card state) (+ j 1))))
                       (+ i 1)))))))))
