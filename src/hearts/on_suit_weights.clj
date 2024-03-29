(ns hearts.on-suit-weights
  (:require [hearts.common-helpers :refer [card->points commonly-high]]))


(defn not-risky∵unplayed
  "NOT USEFUL
  makes low cards in suits that have been played/dealt more valuable
  4 known cards=higher better, 5 known cards, lower=better
  maybe not good enough for encouraging throwing away isolated high cards"
  [curr-suits-known curr-hand]
  (mapv #(- (+ 14 (mod %2 13))
            (* 2 (+ 1 (curr-suits-known (quot %2 13))) %))
        (map #(/ (+ 1 (mod % 13)) 13) curr-hand)  ;decrease by 1/13
        curr-hand))

(defn safe-from-36?
  "Has queen, has passed queen to a finished player, or queen has been played
  (not this round)."
  [round-card-history curr-could-have-36 cp]
  (and (some true? (map (fn [id player-past has-queen]
                          (and (= 2 has-queen)
                               (or (not= -1 player-past)
                                   (= cp id))))
                        (range)
                        round-card-history
                        curr-could-have-36))
       (some #{36} round-card-history)))

(defn probably-safe-from-36?
  "No one left to play 36 who hasn't had the opportunity to play it already.
  Self will never be 1 (1 = maybe has queen)."
  [round-card-history curr-could-have-36]
  (and (not (some false? (map (fn [player-past has-queen]
                           (or (not= 1 has-queen)
                                (< -1 player-past)))
                         round-card-history
                         curr-could-have-36)))
       (some #{36} round-card-history)))

(defn safe-from-broken?
  "player yet to play not broken this suit before"
  [round-card-history curr-suit->player->broken]
  (not (some false? (map (fn [player-past has-broken]
                           (or (not= 1 has-broken)
                                (< -1 player-past)))
                         round-card-history
                         ;;who has broken current suit
                         curr-suit->player->broken))))

(defn early-safe?
  "determine if in safe phase of game (before turn 5, less than 5 cards of this
  suit played, less than 9 cards known and I'm third last player)
  Or, second last player, before turn 8, less than 10 cards known, less than 8
  cards of suit played"
  [num-rounds suits-known cp round-card-history suit]
  (or (and (< (count (filter #{-1} round-card-history)) 3)
           (< num-rounds 5)
           (< ((suits-known cp) suit) 9)
           (< ((suits-known 0) suit) 5))
      (and (< (count (filter #{-1} round-card-history)) 2)
           (< num-rounds 8)
           (< ((suits-known cp) suit) 10)
           (< ((suits-known 0) suit) 8))))

(defn win-avoid-end?
  "is the current winner about to end the game?"
  [winner round-card-history curr-score]
  (> (+ (curr-score winner)
        (reduce #(+ % (card->points %2))
                0
                round-card-history))
     99))

(defn isolated-high? [hand suit early-safe? suits-card-count]
  (if (< (suits-card-count suit) 6)
    (if early-safe?
      (< ((commonly-high hand) suit) 3)
      (< ((commonly-high hand) suit) 2))
    false))

(defn on-suit-weights
  "on-suit-weights provides the weight for every card assuming the player is
  playing a matching suit. The key decision is whether or not to try to win the
  round.
  play to win if one player is shooting moon (including self)
  do not attempt to win queen of spades unless shooting moon
  also need to consider winning card (only need to be lower)
  consider who you passed queen to
  consider who had an opportunity to play queen of spades but did not
  how many players have broken && not discarded
  how many players have discarded"
  [game-state]
  (let [cp (game-state "curr-player")
        suit (quot (second (game-state "winning")) 13)
        round-card-history (first (game-state "card-history"))

        ;;consider winning hand if the following safety checks pass:
        ;;only me left to play?
        last? (< (count (filter #{-1} round-card-history)) 2)

        safe-from-36? (safe-from-36? round-card-history
                                     ((game-state "player->player->could-have-36") cp)
                                     cp)
        probably-safe-from-36?
        (probably-safe-from-36? round-card-history
                                ((game-state "player->player->could-have-36") cp))
        safe-from-broken?
        (safe-from-broken? round-card-history
                           ((game-state "suit->player->broken") suit))
        ;;player yet to play thrown this suit before (only of subtle use)
        #_safe-from-thrown?
        early-safe? (early-safe? (count (game-state "card-history"))
                                 (game-state "suits-known")
                                 cp round-card-history suit)
        ;;consider winning hand anyway:
        win-avoid-end? (win-avoid-end? (first (game-state "winning"))
                                       round-card-history
                                       (first (game-state "turn-depth->(player->cumulative-points)")))
        ;;if safe, consider winning if the following checks pass:
        ;in future, consider if that isolated high card has been broken
        ;(usually already dealt with by early-safe?)
        isolated-high? (isolated-high? ((game-state "player->card-set") cp)
                                       suit early-safe?
                                       (game-state "player->suit->count"))
        shoot-moon? (some #{true} (game-state "shoot-moon"))
        ;in future, consider points relative to end of game
        losing? (> (- ((first (game-state "turn-depth->(player->cumulative-points)")) cp) 15)
                   (apply min (first (game-state "turn-depth->(player->cumulative-points)"))))
        want-win?
        (if (or win-avoid-end?
                shoot-moon?
                (and (or last?
                         safe-from-broken?)
                     (or safe-from-36?
                         (and probably-safe-from-36? losing?))
                     ;in future, could act more risky if losing
                     early-safe?
                     isolated-high?))
          true
          false)]
    ;in future consider barely winning in some circumstances
    (update (game-state "player->card-set") cp
            (fn [hand] (map (fn [card] (if (= suit (quot card 13))
                                         (if (or want-win?
                                                 (< card (second (game-state "winning"))))
                                           card
                                           (/ card 100))
                                         0))
                            hand)))))
