(ns hearts.core)

(def player-cards "player pos assoc with cards in sorted-map (dealer is 0) [{0 8 1 15 2 42 3 46 4 51} {0 1 1 7 2 33 3 43}]"
  (atom []))
(def card-players "card pos assoc with player/dealer in list (length of vector indicates # of decks) ['(0 1) '(0 4)]"
  (atom []))
(def player-suits-broken "player pos then suit pos (y/n for that suit) [[n n n n] [n y y n]]" (atom []))
(def suit-players-broken "suit pos then player pos [[n n n n] [n y y n]]" (atom []))
(def curr-winner (atom [-1 -1]))
(def game-points "hand pos then player pos ([13 4 0 9] [0 13 13 0])" (atom '())) ;could save points of every hand
(def hand-points "player pos" (atom []))

(defn deal [numplayers] "todo: expand beyond 1 deck, 4 players"
  (loop [players (vec (take (inc numplayers) (repeat {})))
         cards (vec (take 52 (repeat '())))
         loose-cards (range 52)
         needy-players (vec (partition 2 (interleave (range 1 (inc numplayers)) (repeat 0))))]
    (if (< (count loose-cards) 1)
      (if (> (count needy-players) 0)
        (throw (Exception. "some players still need cards"))
        (do (reset! player-cards players)
          (reset! card-players cards)))
      (let [chosen-index (rand-int (count needy-players))
            [chosen chosen-count] (needy-players chosen-index)]
        (recur (update players chosen assoc (count (players chosen)) (first loose-cards))
               (update cards (first loose-cards) conj chosen)
               (rest loose-cards)
               (do (prn needy-players)(prn loose-cards)(if (> chosen-count 11)
                                        (vec (keep-indexed #(if (= chosen-index %) nil %2) needy-players))
                                        (update needy-players chosen-index #(list (first %) (inc (second %))))))))))
  (reset! player-suits-broken (apply hash-map (interleave (range 1 (inc numplayers)) (repeat #{}))))
  (reset! suit-players-broken (apply hash-map (interleave (range 1 5) (repeat #{}))))
  (reset! hand-points (apply hash-map (interleave (range 1 (inc numplayers)) (repeat 0))))
  (reset! curr-winner [-1 -1]))

(defn move-card [card from to]
  (prn "from" from "to" to "card" card)
  (when (= from to) (throw (Exception. "a player is passing to itself")))
  (swap! player-cards
         update from (fn [x] (dissoc x (let [pos (reduce-kv #(if (= %3 card) (reduced %2) %) -1 x)]
                                         (if (= -1 pos) (throw (Exception. "card not found")) pos))))





         #(let [pos (java.util.Collections/binarySearch % card compare)]
                        (if (< pos 0) (throw (Exception. "card not found"))
                          (vec (concat (take pos %) (take-last (- (count %) pos 1) %)))))



         #(loop [pos 0]
                        (if (>= pos (count %))
                          (throw (Exception. "card not found"))
                          (if (= (% pos) card)
                            (vec (concat (take pos %) (take-last (- (count %) pos 1) %)))
                            (recur (inc pos)))))

    (fn [player-cards]
      (map (fn [[player cards]]
             (condp = player
               from [from (loop [pos 0]
                            (if (>= pos (count cards))
                              (throw (Exception. "card not found"))
                              (if (= (cards pos) card)
                                (vec (concat (take pos cards) (take-last (- (count cards) pos 1) cards)))
                                (recur (inc pos)))))]
               to [to (conj cards card)] ;doesn't have to be a number for the dealer (primary memory should go elsewhere to provide more information) ;yes it does
               [player cards])) player-cards)))
  (swap! card-players update card (fn [players]
                                    (loop [pos 0]
                                      (if (>= pos (count players))
                                        (throw (Exception. "player not found"))
                                        (if (= (players pos) from)
                                          (vec (concat (take pos players) (list to) (take-last (- (count players) pos 1) players)))
                                          (recur (inc pos))))))))

(defn track-broken [suit card from to] ;suits = 1 2 3 4 ;also need to know about suits played and not broken, as well as high cards played
  (if (and (< card (* suit 13)) (>= card (* (dec suit) 13)))
       nil
       (do (swap! player-suits-broken update from #(conj % suit))
         (swap! suit-players-broken update suit #(conj % from))))
  (move-card card from to))

(defn human-pass [human-player target-player]
  (prn (str "Player " human-player ", what card of yours will you pass to player " target-player "?\n" "Your cards: " (@player-cards human-player)))
  (let [card (clojure.edn/read-string (read-line))] (if (= (type card) Long) (move-card card human-player target-player) (throw (Exception. "not a java.lang.Long")))))

;not based on >1 decks (yes it is b/c new decks will start at 52, all cards will have an unique #, test for = with mod 52) ;no, bad for the card-players map
;fixed
(defn rand-pass [numplayers]
  (doseq [player (rest @player-cards)] ;could make room for humans here
    (let [to (nth (remove #(= (first player) %) (range 1 (inc numplayers))) (rand-int (dec numplayers)))]
      (loop [cards (second player) passes 0]
        (let [card-pos (rand-int (count cards))
              card (nth cards card-pos)]
          (move-card card (first player) to)
          (if (> passes 1) nil
            (recur (keep-indexed #(if (= card-pos %) nil %2) cards) (inc passes)))))
      (prn))))

(defn all-play1-round1 []
  (doseq [player-cards (let [first-player (rand-nth (@card-players 0))]
                         (concat (take-last (- (count @player-cards) first-player) @player-cards)
                                 (rest (take first-player @player-cards))))]
    (prn player-cards)
    (track-broken 1
      (let [card (or (if (empty? (filter #(= (first player-cards) %) (@card-players 0))) nil 0)
                     (let [clubs (filter #(< % 13) (second player-cards))] (if (empty? clubs) nil (rand-nth clubs)))
                     (let [nonpoint (filter #(or (< % 37) (= % 38)) (second player-cards))] (if (empty? nonpoint) nil (rand-nth nonpoint)))
                     (throw (Exception. "player has no card to play in the first round")))]
        (if (and (< card 13) (> card (second @curr-winner))) (reset! curr-winner [(first player-cards) card])) card)
      (first player-cards) 0)))

(defn all-play-another-round []
  )

(defn -main
  [& args]
  (let [name (if (empty? args)
               (do (println "What is your name?") (read-line))
               (first args))]
    (println (str "hi " name))))
