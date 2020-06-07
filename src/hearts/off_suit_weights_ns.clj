(ns hearts.off-suit-weights-ns)

(defn about-to-lose? [point-val curr-player points-total winning]
  (and (not= (points-total curr-player)
             (apply min points-total)) ;not leading and
       ; round winner about to end game
       (> (+ point-val (points-total (first winning))) 99)))

(defn solidarity? [curr-player points-total winning]
    (and (> (- 30 (points-total curr-player))
            (apply min points-total)) ;losing bady and
         (< (+ 10 (points-total curr-player))
            (points-total (first winning)))))  ;round winner is losing more

;;alternative weights (highest will be chosen)
;weight=0 means disallowed
;throw only happens if I'm not able to win hand
;additional logic is required elsewhere for shooting the moon
;many different strategies to take (throw low, throw high, throw to confuse,
;throw based on the number of cards played in suit, throw based on value of cards
;remaining in suit (lower or higher than my cards), throw high spades and hearts)
;this function should focus on:shooting the moon, stopping shooting the moon,
;avoiding loss, seeking win, throwing isolated high
;in future, should consider who is likely to win hand, not curr winner
(defn off-suit-weights [game-state]
  (let [curr-player (game-state "curr-player")
        points-total (first (game-state "points-history"))
        winning (game-state "winning")
        hand-points (apply mapv - (take 2 (game-state "points-history")))
        hand (game-state "curr-hand")]
    (map #(cond ;no shoot the moon strategy for the queen of spades
            (= % 36)
            (if (or (about-to-lose? 13 curr-player points-total winning)
                    (solidarity? curr-player points-total winning))
              0.0001 1000)
            (or (= % 37) (= % 38))
            (if (((game-state "player-cards") 0) 36) ;36 has been played
              %2 750)
            (> % 38)
            (+ % (cond (and (< 1 (count (filter zero? points-total))) ;potential shoot moon
                            (not= (apply max hand-points)
                                  (hand-points curr-player)) ;I'm not shooting moon
                            (not= (apply max hand-points)
                                  (hand-points (first winning)))) ;round winner is not shooting moon
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
                     ((game-state "player-suits-card-count") (quot card 13)) ;by default throw high cards from suits with few cards
                     breakage)))
              hand))))
