(ns hearts.core-test
  (:require [clojure.test :refer :all]
            [hearts.core :refer :all])
  (:use [clojure.set :only [union intersection difference]]))
;;test all with (clojure.test/run-tests 'hearts.core-test) or lein test

(deftest deal-test
  (testing "Each player receives cards."
    (is (= (count ((start-game) "player-cards"))
           5))
    (is (apply =
               13
               (map count (rest ((start-game) "player-cards")))))
    (is (= (sort (apply concat (rest ((start-game) "player-cards"))))
           (range 52)))
    (is (= [0 13 13 13 13]
           (reduce #(update % %2 inc)
                   [0 0 0 0 0]
                   ((start-game) "card-players")))))
  (testing "Variables are initialized."
    (is (= (sort '("player-cards", "card-players", "pass-direction", "passed",
                   "card-history", "points-history", "human", "first-players"))
           (sort (keys (start-game)))))))

(deftest pass-test
  (let [pre-pass (start-game)
        post-pass (pass pre-pass)
        post-pass2 (pass post-pass)
        post-pass3 (pass post-pass2)
        post-pass4 (pass post-pass3)
        post-pass5 (pass post-pass4)]
    (testing "Each player passes 3 cards."
             (is (= [0 10 10 10 10]
                    (mapv (fn [old-hand new-hand]
                            (count (filter (fn [old-card]
                                             (new-hand old-card))
                                           old-hand)))
                          (pre-pass "player-cards")
                          (post-pass "player-cards"))))
             (is (= [0 10 10 10 10]
                    (mapv (fn [old-hand new-hand]
                            (count (filter (fn [old-card]
                                             (new-hand old-card))
                                           old-hand)))
                          (post-pass "player-cards")
                          (post-pass2 "player-cards")))))
    (testing "Players pass in different directions."
             (is (= [-1 4 1 2 3]
                    (mapv (fn [source-hand]
                            (reduce-kv (fn [acc key hand]
                                            (if (= 3 (count (intersection hand source-hand)))
                                              key acc))
                                       -1
                                       (post-pass "player-cards")))
                          (pre-pass "player-cards"))))
             (is (= [3 3 3 3]
                    (let [diff (partition 2 (interleave (pre-pass "card-players") (post-pass "card-players")))]
                      (vector (count (filter #(and (= 1 (first %)) (= 4 (second %))) diff))
                              (count (filter #(and (= 2 (first %)) (= 1 (second %))) diff))
                              (count (filter #(and (= 3 (first %)) (= 2 (second %))) diff))
                              (count (filter #(and (= 4 (first %)) (= 3 (second %))) diff))))))
             (is (= [-1 2 3 4 1]
                    (mapv (fn [source-hand]
                            (reduce-kv (fn [acc key hand]
                                         (if (= 3 (count (intersection hand source-hand)))
                                           key acc))
                                       -1
                                       (post-pass2 "player-cards")))
                          (post-pass "player-cards"))))
             (is (= [3 3 3 3]
                    (let [diff (partition 2 (interleave (post-pass "card-players") (post-pass2 "card-players")))]
                      (vector (count (filter #(and (= 1 (first %)) (= 2 (second %))) diff))
                              (count (filter #(and (= 2 (first %)) (= 3 (second %))) diff))
                              (count (filter #(and (= 3 (first %)) (= 4 (second %))) diff))
                              (count (filter #(and (= 4 (first %)) (= 1 (second %))) diff))))))
             (is (= [-1 3 4 1 2]
                    (mapv (fn [source-hand]
                            (reduce-kv (fn [acc key hand]
                                         (if (= 3 (count (intersection hand source-hand)))
                                           key acc))
                                       -1
                                       (post-pass3 "player-cards")))
                          (post-pass2 "player-cards"))))
             (is (= [3 3 3 3]
                    (let [diff (partition 2 (interleave (post-pass2 "card-players") (post-pass3 "card-players")))]
                      (vector (count (filter #(and (= 1 (first %)) (= 3 (second %))) diff))
                              (count (filter #(and (= 2 (first %)) (= 4 (second %))) diff))
                              (count (filter #(and (= 3 (first %)) (= 1 (second %))) diff))
                              (count (filter #(and (= 4 (first %)) (= 2 (second %))) diff))))))
             (is (= [-1 -1 -1 -1 -1]
                    (mapv (fn [source-hand]
                            (reduce-kv (fn [acc key hand]
                                         (if (= 3 (count (intersection hand source-hand)))
                                           key acc))
                                       -1
                                       (post-pass4 "player-cards")))
                          (post-pass3 "player-cards"))))
             (is (= [13 13 13 13]
                    (let [diff (partition 2 (interleave (post-pass3 "card-players") (post-pass4 "card-players")))]
                      (vector (count (filter #(and (= 1 (first %)) (= 1 (second %))) diff))
                              (count (filter #(and (= 2 (first %)) (= 2 (second %))) diff))
                              (count (filter #(and (= 3 (first %)) (= 3 (second %))) diff))
                              (count (filter #(and (= 4 (first %)) (= 4 (second %))) diff))))))
             (is (= [-1 4 1 2 3]
                    (mapv (fn [source-hand]
                            (reduce-kv (fn [acc key hand]
                                         (if (= 3 (count (intersection hand source-hand)))
                                           key acc))
                                       -1
                                       (post-pass5 "player-cards")))
                          (post-pass4 "player-cards"))))
             (is (= [3 3 3 3]
                    (let [diff (partition 2 (interleave (post-pass4 "card-players") (post-pass5 "card-players")))]
                      (vector (count (filter #(and (= 1 (first %)) (= 4 (second %))) diff))
                              (count (filter #(and (= 2 (first %)) (= 1 (second %))) diff))
                              (count (filter #(and (= 3 (first %)) (= 2 (second %))) diff))
                              (count (filter #(and (= 4 (first %)) (= 3 (second %))) diff)))))))))

(deftest first-round-init-test
  (testing "Initialize information needed at the beginning of each hand."
    (let [pre-init (pass (start-game))
          post-init (first-round-init pre-init)
          post-init-static
          (first-round-init {"points-history" '([0 0 0 0 0] [0 0 0 0 0])
                             "card-history" '()
                             "first-players" '()
                             "pass-direction" 1
                             "human" [0 0 0 0 0]
                             "player-cards" [#{}
                                             #{0 2 4 9 14 16 21 25 27 31 37 49 51}
                                             #{8 15 18 19 20 29 35 36 38 42 46 48 50}
                                             #{1 11 12 13 23 24 32 33 34 39 41 43 45}
                                             #{3 5 6 7 10 17 22 26 28 30 40 44 47}]
                             "card-players" [1 3 1 4 1 4 4 4 2 1 4 3 3 3 1 2 1 4
                                             2 2 2 1 4 3 3 1 4 1 4 2 4 1 3 3 3 2
                                             2 1 2 3 4 3 2 3 4 3 2 4 2 1 2 1]
                             "passed" []})]
      (is (= #{"winning" "playable" "curr-player" "player-suits" "suits-known"
               "suit-players-broken"}
             (difference (set (keys post-init)) (set (keys pre-init)))))
      (is (= [1 0] (post-init-static "winning")))
      (is (= 2 (post-init-static "curr-player")))
      (is (= 0 (((post-init-static "player-cards") 0) 0)))))) ;dealer has 2 of clubs

(deftest card-suit-rarity-test
  (testing "Create weights for inverting card value given more known cards."
    (is (= (map #(format "%.2f" (float %))
                (card-suit-rarity {"suits-known" [[8 4 0 1]]
                                   "curr-player" 0
                                   "player-cards-sort" [0 1 2 4 6 7 9 12 15 16 22 25 48]}))
           '("12.62" "12.23" "11.85" "11.08" "10.31" "9.92" "9.15" "8.00" "13.69"
             "13.92" "15.31" "16.00" "19.92")))))

(deftest throw-weights
  (testing "Create weights for "))
#_(deftest subsequent-play-card-test
  (testing "Card is given to dealer. Capable of being called 3 times sequentially."
    (let [pre-play (first-round-init (pass (deal (start-game))))
          discard1 (subsequent-play-card (choice-init pre-play))
          discard2 (subsequent-play-card (choice-init discard1))
          discard3 (subsequent-play-card (choice-init discard2))]
      (is (= 1 (count ((pre-play "player-cards") 0))))
      (is (= 2 (count ((discard1 "player-cards") 0))))
      (is (= 3 (count ((discard2 "player-cards") 0))))
      (is (= 4 (count ((discard3 "player-cards") 0)))))
    #_(let [pre-play-static {"player-cards" [#{0}
                                           #{1 2 3 6 7 9 16 17 25 37 38 42}
                                           #{5 10 18 19 20 23 31 32 39 40 46 50 51}
                                           #{12 13 14 15 27 28 30 36 41 43 44 48 49}
                                           #{4 8 11 21 22 24 26 29 33 34 35 45 47}],
                           "card-players" [0 1 1 1 4 2 1 1 4 1 2 4 3 3 3 3 1 1 2
                                           2 2 4 4 2 4 1 4 3 3 4 3 2 2 4 4 4 3 1
                                           1 2 2 3 1 3 3 4 2 4 3 3 2 2],
                           "pass-direction" 1,
                           "card-history" '(([-1 0 -1 -1 -1])),
                           "winning" [1 0],
                           "playable" [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                                       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 0
                                       0 0 0 0 0 0 0 0 0 0],
                           "first-players" '((1)),
                           "human" [0 0 0 0 0],
                           "curr-player" 2,
                           "passed" [],
                           "points-history" '([0 0 0 0 0] [0 0 0 0 0] [0 0 0 0 0])}
          discard1 (subsequent-play-card (choice-init pre-play-static))
          discard2 (subsequent-play-card (choice-init discard1))
          discard3 (subsequent-play-card (choice-init discard2))]
      (is (= #{0 11 10 12} ((discard3 "player-cards") 0))))))

#_(def pre-play {"player-cards" [#{0} #{7 18 19 32 33 34 35 39 41 42 43 47 48} #{1 6 9 10 11 16 20 25 26 27 28 29 30} #{2 3 4 5 8 12 13 15 22 38 46 51} #{14 17 21 23 24 31 36 37 40 44 45 49 50}], "player-suits" [[0 0 0 0] [1 2 4 6] [5 3 5 0] [7 3 1 2] [0 5 3 5]], "card-players" [0 2 3 3 3 3 2 1 3 2 2 2 3 3 4 3 2 4 1 1 2 4 3 4 4 2 2 2 2 2 2 4 1 1 1 1 4 4 3 1 4 1 1 1 4 4 3 1 1 4 4 3], "suits-known" [[0 0 0 0] [1 2 4 6] [5 3 5 0] [7 3 1 2] [0 5 3 5]], "pass-direction" 1, "card-history" '(([-1 -1 -1 0 -1])), "winning" [3 0], "playable" [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0], "first-players" '((3)), "human" [0 0 0 0 0], "curr-player" 4, "passed" [], "points-history" '([0 0 0 0 0] [0 0 0 0 0] [0 0 0 0 0])})
