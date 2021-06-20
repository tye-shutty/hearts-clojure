(ns hearts.off-suit-weights-ns
  (:require [hearts.common-helpers :refer [#_shoot-moon?
                                           card->points]]))

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

(defn broken-weighter 
  "multiplicative weights for suits based on broken status, little weight to suits broken by everyone else
   ((game-state 'suit->player->broken') (quot card 13))"
  [players-broken]
  (condp = (apply + players-broken)
                 0 1
                 1 1.4
                 2 1.4
                 3 0.2))

(defn points-weighter
  "multiplicative weights of (1+points value). If being nice, subtract from points cards"
  [card punish?]
  (- (+ 1 (card->points card))
     (if (> (card->points card) 0)
       (if punish?
         0
         (if (= 36 card) 40 20))
       0)))

(defn off-suit-weights
  "Weights indicate whether to punish or not (highest will be chosen). index is card.
   weight=0 means disallowed
   throw only happens if I'm not able to win hand
   many different strategies to take (throw low, throw high, throw to confuse,
   throw based on the number of cards played in suit, throw based on value of cards
   remaining in suit (lower or higher than my cards), throw high spades and hearts)
   
   This function should focus on:shooting the moon, stopping shooting the moon,
   avoiding loss, seeking win, throwing isolated high
   in future, should consider who is likely to win hand, not curr winner"
  [game-state]
  nil
  #_(let [cp (game-state "curr-player")
        points-total (first (game-state "turn-depth->(player->cumulative-points)"))
        winning (game-state "winning")
        hand-points (apply mapv - (take 2 (game-state "turn-depth->(player->cumulative-points)")))
        hand (game-state "curr-hand")
        about-to-lose? (about-to-lose? 13 cp points-total winning)
        solidarity? (solidarity? cp points-total winning)]
    
    (map #(cond ;no shoot the moon strategy for the queen of spades
            (= % 36)
            (if (or )
              0.0001 1000)
            (or (= % 37) (= % 38))
            (if (((game-state "player->card-set") 0) 36) ;36 has been played
              %2 750)
            (> % 38)
            (+ % (cond (and
                       (+ 500 %2)
                       (and (not= cp (first winning)) ;about to lose
                            (> (points-total (first winning)) 95))
                       0.001
                       (or (= cp (first winning))
                           (< (points-total (first winning))
                              (+ 20 (points-total cp)))) ;don't kick a man while he's down, unless you're winning
                       (* 1.2 %2)
                       :else %2))
            :else %2)
         hand
         (map (fn [card]
                
                (let []
                  #_(/ (mod card 13)
                     ((game-state "player->suit->count") (quot card 13)) ;by default throw high cards from suits with few cards
                     breakage)))
              hand)))))
