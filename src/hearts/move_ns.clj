(ns hearts.move-ns)

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
