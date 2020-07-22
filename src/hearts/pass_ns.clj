(ns hearts.pass-ns
  (:require [hearts.common-helpers :refer [move-card commonly-high]]))

(defn human-select-pass [player] nil)
#_(defn human-pass [human-player target-player]
  (prn (str "Player " human-player ", what card of yours will you pass to player " target-player "?\n" "Your cards: " (@player-cards human-player)))
  (let [card (clojure.edn/read-string (read-line))] (if (= (type card) Long) (move-card card human-player target-player) (throw (Exception. "not a java.lang.Long")))))

(defn keep-queen [player score num-spades pass-dir]
  (if (or (> num-spades 5) ;keep queen if have 6+ spades ;in future consider only low spades?
          (and (> num-spades 3) ;keep queen if have 4+ spades AND
               (or (= pass-dir 1) ; if it would be played after you play OR
                   (reduce #(or %1 (> %2 (+ 20 (score (- player 1))))) ;losing
                           false
                           score))))
    true false))
(defn unbroken-highest
  "Returns pair of numbers for every suit. First is number of cards in a continuous sequence
  of difference=1 from the highest, the second is the value of the highest card. 0 and -2 if
  no cards in suit. Card value 0-12."
  [hand]
  (reduce (fn [suits card]
            (let [suit (quot card 13)
                  card (mod card 13)]
              (update suits
                      suit
                      (fn [pair]
                        (if (= (inc (second pair)) card)
                          [(inc (first pair)) card]
                          [1 card])))))
          [[0 -2] [0 -2] [0 -2] [0 -2]] ;[num high, last card] -suit pos
          hand))

(defn shoot-moon [hand]
  (let [unbroken-highest (unbroken-highest hand)
        commonly-high (commonly-high hand)
        high (map (fn [pair estimate]
                    (+ (condp = (second pair)
                           12 (first pair)
                           11 (/ (first pair) 1.25) ;less value for 2nd highest
                           0)
                       (/ ((fn [diff] (if (< diff 0) 0 diff))
                               (- estimate (first pair)))
                          2))) ;quarter value for other cards above 7
                  unbroken-highest
                  commonly-high)]
    (and (> (apply + high) 6)
         (> (apply + (butlast high)) 5)))) ;not just hearts

(defn moon-weights [hand]
  (->> (mapv #(vec (reverse %))
             (reduce (fn [suits card]
                       (update suits
                               (quot card 13)
                               (fn [suit] (conj suit [0 card]))))
                     [[][][][]]
                     hand))
       (mapv (fn [suit]
               (-> (fn [key pair]
                     (as-> #(- (second (suit (dec key)))
                               (second pair)
                               1) v
                           (constantly (condp = key
                                              0 (- 12 (mod (second pair) 13))
                                              (v)))
                           (update pair
                                   0
                                   v)))
                   (keep-indexed suit)
                   (vec))))
       (mapv (fn [suit]
               (-> (fn [key pair]
                     (as-> (fn [sum key] (+ sum (first (suit key)))) v
                           #(reduce v (first pair) (range key))
                           (constantly (condp = key
                                              0 (first pair)
                                              (v)))
                           (update pair 0 v)))
                   (keep-indexed suit)
                   (vec))))
       (apply concat)))

(defn inc-pass-dir [player pass-dir n-players]
  (condp = (+ player pass-dir)
         n-players 1
         0 (- n-players 1)
         (inc n-players) 2
         (+ player pass-dir)))

(defn special-card-weights [keep-queen]
  {36 (if keep-queen 0 100)
   37 #_king-spades (if keep-queen 11 90)
   38 #_ace-spades (if keep-queen 12 90)
   0 #_two-clubs 13})

(defn all-card-weights [keep-queen]
  (vec (concat (take 26 (repeat 1))
               (take 10 (repeat (if keep-queen 1/2 1))) ;maybe keep low spades
               '(1 1 1)
               (take 3 (repeat 1/2)) ;keep low hearts
               '(1 1 1)
               (take 7 (repeat 2))))) ;throw high hearts)

(defn pass-weights [shoot-moon hand all-card-weights special-card-weights
                    suit-not-nums]
  (if shoot-moon
    (moon-weights hand)
    (map (fn [card] (vector (* (all-card-weights card)
                               ;default weight is the value of the card
                               (inc (special-card-weights card (mod card 13)))
                               (* (suit-not-nums (quot card 13)) 2))
                            card))
         hand)))

(defn pass3 [weights new-game-state player dest shoot-moon]
  (loop [top3 (take 3 (sort #(> (first %) (first %2))
                            weights)) ;[weight card]
         new-game-state (update new-game-state
                                "passed"
                                (fn [passed]
                                  (conj passed {[player dest] '()})))]
    (if (empty? top3) new-game-state
      (recur (rest top3)
             (let [card (second (first top3))
                   new-game-state (update new-game-state
                                          "shoot-moon"
                                          (fn [shooters]
                                            (update shooters
                                                    player
                                                    (constantly shoot-moon))))
                   new-game-state (update new-game-state
                                          "passed"
                                          (fn [passed]
                                            (update passed
                                                    [player dest]
                                                    #(cons card %))))]
               (move-card card player dest new-game-state))))))

;player determines if shoot moon before passing, not after
(defn pass [game-state]
  (-> (if (= (game-state "pass-direction") 0)
        game-state
        (let [n-players (count (game-state "player-cards"))]
          (loop [player (- n-players 1)
                 new-game-state game-state]
            (if (< player 1)
              new-game-state
              (recur (- player 1)
                     (let [dest (inc-pass-dir player
                                              (game-state "pass-direction")
                                              n-players)]
                       (if (= ((game-state "human") player) 1)
                         (human-select-pass player)
                         (let [hand (sort ((game-state "player-cards") player))
                               suit-nums (reduce #(update %1 (quot %2 13) inc)
                                                 [0 0 0 0]
                                                 hand) ;num cards in each suit
                               ; num cards not in each suit
                               suit-not-nums (mapv #(- 14 %) suit-nums)
                               score (first (game-state "points-history"))
                               shoot-moon (shoot-moon hand)
                               keep-queen (keep-queen player score
                                                      (suit-nums 2)
                                                      (game-state "pass-direction"))
                               special-card-weights (special-card-weights keep-queen)
                               all-card-weights (all-card-weights keep-queen)
                               weights (pass-weights shoot-moon
                                                     hand
                                                     all-card-weights
                                                     special-card-weights
                                                     suit-not-nums)]
                           (pass3 weights
                                  new-game-state
                                  player
                                  dest
                                  shoot-moon)))))))))
      (update "pass-direction" #(condp = %, -1 1, 1 2, 2 0, 0 -1))))
