(ns hearts.core)

(def deck (vec (for [suit [:h :s :c :d] val (range 1 14)] [suit val])))

(def player-cards "player key assoc with cards" (atom ^clojure.lang.PersistentArrayMap {}))

(def players-cards-count "player key assoc with num cards" (atom ^clojure.lang.PersistentArrayMap {}))

(defn deal [] (reset! player-cards
                      (reduce #(assoc (let [player (rand-nth (vec @players-cards-count))]
                                        (if (= (second player) card-limit)))) {} deck)))

;no use?
(def card-player)

(defn trade-strategy [])

(def card-values-1 "for trade-strategy" (reduce #(cond (= (...))) {} deck))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [name (if (empty? args)
               (do (println "What is your name?") (read-line))
               (first args))]
    (println (str "hi " name))))
