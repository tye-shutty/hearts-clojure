(ns hearts.common-helpers)

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

(defn card->points [card]
  (cond
    (= card 36) 13
    (> card 38) 1
    :else 0))

(defn commonly-high
  "Returns list of number of cards over 7 in each suit."
  [hand]
  (reduce (fn [acc card]
            (if (> (mod card 13) 7)
              (update acc (quot card 13) inc) acc))
          [0 0 0 0]
          hand))
