(ns hearts.core-test
  (:require [clojure.test :refer :all]
            [hearts.core :refer :all])
  (:use [clojure.set :only [union intersection difference]]))
;;test all with (clojure.test/run-tests 'hearts.core-test) or lein test

(deftest deal-test
  (testing "Each player receives cards."
    (is (= (count ((game-start) "player-cards"))
           5))
    (is (apply =
               13
               (map count (rest ((game-start) "player-cards")))))
    (is (= (sort (apply concat (rest ((game-start) "player-cards"))))
           (range 52)))
    (is (= [0 13 13 13 13]
           (reduce #(update % %2 inc)
                   [0 0 0 0 0]
                   ((game-start) "card-players")))))
  (testing "Variables are initialized."
    (is (= (sort '("player-cards", "card-players", "pass-direction", "passed",
                   "card-history", "points-history", "human"))
           (sort (keys (game-start)))))))

(deftest pass-test
  (let [pre-pass (game-start)
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
    (let [pre-init (pass (game-start))
          post-init (first-round-init pre-init)]
      (is (= #{"first-player", "curr-player", "winning", "broken"}
             (difference (set (keys post-init)) (set (keys pre-init))))))))
