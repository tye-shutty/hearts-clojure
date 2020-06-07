(ns hearts.on-suit-weights)

;;NOT USEFUL
;; makes low cards in suits that have been played/dealt more valuable
;;4 known cards=higher better, 5 known cards, lower=better
; maybe not good enough for encouraging throwing away isolated high cards
(defn not-risky∵unplayed [curr-suits-known curr-hand]
  (mapv #(- (+ 14 (mod %2 13))
            (* 2 (+ 1 (curr-suits-known (quot %2 13))) %))
        (map #(/ (+ 1 (mod % 13)) 13) curr-hand)  ;decrease by 1/13
        curr-hand))

;;only safe if know who has it
(defn safe-from-36 [round-card-history curr-could-have-36 cp]
  (some true? (map (fn [id player-past has-queen]
                     (and (= 2 has-queen)
                          (or (not= -1 player-past)
                              (= cp id))))
                   (range)
                   round-card-history
                   curr-could-have-36)))

;;self will never be 1 (1 = maybe has queen)
(defn probably-safe-from-36 [round-card-history curr-could-have-36]
  (not (some false? (map (fn [player-past has-queen]
                           (or (not= 1 has-queen)
                                (< -1 player-past)))
                         round-card-history
                         curr-could-have-36))))

;;player yet to play broken this suit before
(defn safe-from-broken [round-card-history curr-suit-players-broken]
  (not (some false? (map (fn [player-past has-broken]
                           (or (not= 1 has-broken)
                                (< -1 player-past)))
                         round-card-history
                         ;;who has broken current suit
                         curr-suit-players-broken))))

;;determine if in safe phase of game (before turn 5, less than 5 cards of this
;suit played, less than 9 cards known and I'm third last player)
;;Or, second last player, before turn 8, less than 10 cards known, less than 8
;cards of suit played
(defn safe-ish [num-rounds suits-known cp round-card-history suit]
  (or (and (< (count (filter #{-1} round-card-history)) 3)
           (< num-rounds 5)
           (< ((suits-known cp) suit) 9)
           (< ((suits-known 0) suit) 5))
      (and (< (count (filter #{-1} round-card-history)) 2)
           (< num-rounds 8)
           (< ((suits-known cp) suit) 10)
           (< ((suits-known 0) suit) 8))))

;;play to win if one player is shooting moon (including self)
;;do not attempt to win queen of spades unless shooting moon
;;also need to consider winning card (only need to be lower)
;;consider who you passed queen to
;;consider who had an opportunity to play queen of spades but did not
;;how many players have broken && not discarded
;;how many players have discarded
(defn on-suit-weights [game-state]
  (let [cp (game-state "curr-player")
        suit (quot (second (game-state "winning")) 13)
        round-card-history (first (game-state "card-history"))
        ;;only me left
        safe (< (count (filter #{-1} round-card-history)) 2)
        safe-from-36 (safe-from-36 round-card-history
                                   ((game-state "could-have-36") cp)
                                   cp)
        probably-safe-from-36
        (probably-safe-from-36 round-card-history
                               ((game-state "could-have-36") cp))
        safe-from-broken
        (safe-from-broken round-card-history
                          ((game-state "suit-players-broken") suit))
        ;;player yet to play thrown this suit before (only of subtle use)
        #_safe-from-thrown
        safe-ish (safe-ish (count (game-state "card-history"))
                           (game-state "suits-known")
                           cp round-card-history suit)
        ]
    nil))
