(ns hearts.freq-core-test
  "For tests of distributions."
  (:require [hearts.core :refer [create-game]]
             [hearts.pass-ns :refer [pass]]))


(defn shoot-moon-occurrence
  "Returns number of turns until of shoot-moon strategy first chosen."
  []
  (loop [g2 (pass (create-game))
         count 0]
    (if (or (reduce-kv (fn [acc k v] (or v acc)) false (g2 "shoot-moon")) (> count 100))
      [count (g2 "shoot-moon") (map #(map (fn [x] (mod x 13)) %) (g2 "player->card-set"))]
      (recur (pass (create-game)) (inc count)))))

(defn average-shoot-moon-occ []
  (float (/ (loop [count 0
                   ave 0]
              (if (= count 1000)
                ave
                (recur (inc count)
                  (+ ave (first (shoot-moon-occurrence))))))
            1000)))
