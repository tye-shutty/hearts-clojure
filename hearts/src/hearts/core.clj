(ns hearts.core)

(def player-cards "player key assoc with cards (dealer is 0) {0 [51 23 42 46 8] 1 [1 7 45 33]}"
  (atom ^clojure.lang.PersistentArrayMap {}))

(def card-players "card key assoc with player/dealer (length of vector indicates # of decks) {0 [0 1] 1 [1 4]}"
  (atom ^clojure.lang.PersistentArrayMap {}))

(defn deal [numplayers] "todo: expand beyond 1 deck, 4 players"
  (loop [players (apply hash-map (interleave (range (inc numplayers)) (repeat [])))
         cards (apply hash-map (interleave (range 52) (repeat [])))
         loose-cards (range 52)
         needy-players (vec (partition 2 (interleave (range 1 (inc numplayers)) (repeat 0))))]
    (if (< (count loose-cards) 1)
      (if (> (count needy-players) 0)
        (throw (Exception. "some players still need cards"))
        (do (reset! player-cards players)
          (reset! card-players cards)))
      (let [chosen-index (rand-int (count needy-players))
            [chosen chosen-count] (needy-players chosen-index)]
        (recur (update players chosen (fn [xs] (conj xs (first loose-cards))))
               (update cards (first loose-cards) (fn [xs] (conj xs chosen)))
               (rest loose-cards)
               (do (prn needy-players)(prn loose-cards)(if (> chosen-count 11)
                                        (vec (keep-indexed #(if (= chosen-index %) nil %2) needy-players))
                                        (update needy-players chosen-index #(list (first %) (inc (second %)))))))))))


;;not idempotent
(defn move-card [card from to]
  (do (swap! player-cards
        (fn [player-cards]
          (into {}
            (map (fn [[player cards]]
                   (condp = player
                     from [from (loop [pos 0]
                                  (if (>= pos (count cards))
                                    (throw (Exception. "card not found"))
                                    (if (= (cards pos) card)
                                      (concat (take pos cards) (take-last (- (count cards) pos 1) cards))
                                      (recur (inc pos)))))]
                     to [to (conj cards card)]
                     [player cards])) player-cards))))
    (swap! card-players update card (fn [players]
                                      (loop [pos 0]
                                        (if (>= pos (count players))
                                          (throw (Exception. "player not found"))
                                          (if (= (players pos) from)
                                            (vec (concat (take pos players) (list to) (take-last (- (count players) pos 1) players)))
                                            (recur (inc pos)))))))))

;no use?


(defn -main
  [& args]
  (let [name (if (empty? args)
               (do (println "What is your name?") (read-line))
               (first args))]
    (println (str "hi " name))))

; (defn notuseful "returns player and, if present, the nth of card" [card]
;   (reduce (fn [[player cards]]
;             (if-let [found (reduce #(if (= card %2) (reduced [true (second %)]) [false (inc (second %))]) [false 0] cards)]
;               (reduced [player found]) [(inc (% 0))])) [0] player-cards))