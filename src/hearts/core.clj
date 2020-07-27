(ns hearts.core
  (:gen-class)
  (:require [hearts.on-suit-weights :refer [on-suit-weights]]
        [hearts.pass-ns :refer [pass]]
        [hearts.common-helpers :refer [move-card]]
        [hearts.off-suit-weights-ns :refer [off-suit-weights]]))
;(use ['hearts.core-test :refer :all])
#_(def player-suits-broken "player pos then suit pos (y/n=1/0 for that suit) [[1 1 1 1] [0 1 1 0]]" (atom []))

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
                {"player-cards" players ;"player/dealer pos assoc with cards in sorted-set (dealer is 0, other decks start at 52) [#{8 15 42 46 51} #{1 7 33 43}]. aces high"
                 "card-players" cards ;"card pos assoc with player/dealer num [1 4]"
                 "passed" {[0 0] '()} ;player pos of this hand's passed cards {[1 4] '(0 51 40) [4 1] '(37 38 12)} ;{[source dest] '(cards)}
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
  (deal {;;the cumulative points at the end of each hand, most recent first,
         ;then player pos '([0 13 4 0 9] [0 0 13 13 0]) ;last element is all
         ;zeros
         "points-history" '([0 0 0 0 0])
         ;;"hand pos then round pos (most recent to oldest), then player pos,
         ;'(([-1 0 -1 -1 12]))"
         "card-history" '()
         ;;same structure as card-history to save order of play
         "first-players" '()
         ;;"-1=right, 1=left(clockwise, the order of the player-cards),
         ;2=across(for now), 0=no passing"
         "pass-direction" -1
         "shoot-moon" [false false false false false]
         ;;"player/dealer pos y/n [0 0 0 0 1]"
         "human" [0 0 0 0 0]}))

(defn first-round-init [game-state]
  (let [fp ((game-state "card-players") 0)
        hist-init (update [-2 -1 -1 -1 -1] fp (constantly 0))
        player-cards-sort (mapv #(vec (sort %)) (game-state "player-cards"))
        player-suits (mapv (fn [hand] (reduce #(update %1 (quot %2 13) inc)
                                              [0 0 0 0] hand))
                           player-cards-sort)
        game-state (move-card 0 fp 0 game-state)]
    (conj game-state
          {"first-players" (cons (list fp) (game-state "first-players"))
           "card-history" (cons (list hist-init) (game-state "card-history"))
           "curr-player" (if (= fp 4) 1 (+ fp 1))
           ; only for first round and when starting new rounds
           "playable" (vec (concat (take 36 (repeat 1)) '(0 1 1) (take 13 (repeat 0))))
           "winning" [fp 0]  ;player, card
           "player-suits-card-count" player-suits
           ;"suit pos then player pos (y/n = 1/0) [[0 0 0 0 0] [0 1 1 0 0]]"
           "suit-players-broken" (vec (take 4 (repeat (vec (take 5 (repeat 0))))))
           ;players that might have queen of spades, based on assumption they'd
           ;play it asap also consider gameending implications of playing as a
           ;reason why they might retain it. 2=certainty (I have it or I passed
           ;it), 0=missed opportunity to play it (probably doesn't have it)
           "could-have-36"
           (vec (keep-indexed (fn [player queen-holders]
                           (let [from-to (first (filter #(= player (first %))
                                                 (keys (game-state "passed"))))]
                             (if (some #{36} ((game-state "passed") from-to))
                               (update queen-holders (second from-to) (constantly 2))
                               (if (= player ((game-state "card-players") 36))
                                 (update queen-holders player (constantly 2))
                                 queen-holders))))
                         (vec (take 5 (repeat [0 1 1 1 1])))))
           "suits-known" (mapv #(mapv + % (player-suits 0)) player-suits)})))

((first-round-init (pass (start-game))) "could-have-36")

(defn subsequent-choice
  "Returns best card to play when not leading choice"
  [game-state]
  (let [curr-hand (vec (sort ((game-state "player-cards")
                                          (game-state "curr-player"))))
        game-state (conj game-state {"curr-hand" curr-hand})
        suit (quot (second (game-state "winning")) 13)
        weights (if (= 0 (((game-state "player-suits-card-count")
                                       (game-state "curr-player"))
                                       suit))
                  (off-suit-weights game-state)
                  (on-suit-weights game-state))]
    (.indexOf weights (apply max weights))))  ;returns first match (low bias)

;;returns card
#_(defn leading-choice [game-state]
  (let [on-suit-weight (on-suit-weight game-state)
        suit-count ((game-state "player-suits-card-count")
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

#_(defn subsequent-round-init [game-state]
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



#_(defn -main
  [& args]
  (let [name (if (empty? args)
               (do (println "What is your name?") (read-line))
               (first args))]
    (println (str "hi " name))))
