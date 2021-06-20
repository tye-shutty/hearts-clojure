(ns hearts.core-test
  (:require [clojure.test :refer [testing is deftest]]
            [hearts.common-helpers :refer [commonly-high]]
            [hearts.pass-ns :refer [pass
                                    shoot-moon
                                    moon-weights
                                    unbroken-highest
                                    keep-queen]]
            [hearts.off-suit-weights-ns :refer [off-suit-weights]]
            [hearts.on-suit-weights :refer [on-suit-weights
                                            not-risky∵unplayed
                                            safe-from-36?
                                            probably-safe-from-36?
                                            safe-from-broken?
                                            #_safe-ish]]
            [hearts.core :refer [create-game organize-new-hand]]
            [clojure.set :refer [intersection difference]]
            [clojure.tools.trace :refer [trace]]))
;;test all with (clojure.test/run-tests 'hearts.core-test) or lein test
;; (trace/trace-ns 'hearts.core-test)

(defn suitnum->handnum [suits]
  (flatten (map (fn [s floor]
                  (map (fn [c] (+ c floor)) s)) suits '(0 13 26 39))))

(deftest deal-test
  (testing "Each player receives cards."
    (is (= (count ((create-game) "player->card-set"))
           5))
    (is (apply =
               13
               (map count (rest ((create-game) "player->card-set")))))
    (is (= (sort (apply concat (rest ((create-game) "player->card-set"))))
           (range 52)))
    (is (= [0 13 13 13 13]
           (reduce #(update % %2 inc)
                   [0 0 0 0 0]
                   ((create-game) "card->player")))))
  (testing "Variables are initialized."
    (is (= (sort '("player->card-set", "card->player", "pass-direction", "passer->passee",
                   "hand->round->player->card", "turn-depth->(player->cumulative-points)", "human", "hand->round->player->play-order",
                   "shoot-moon"))
           (sort (keys (create-game)))))))

(deftest pass-test
  (let [pre-pass (create-game)
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
                          (pre-pass "player->card-set")
                          (post-pass "player->card-set"))))
             (is (= [0 10 10 10 10]
                    (mapv (fn [old-hand new-hand]
                            (count (filter (fn [old-card]
                                             (new-hand old-card))
                                           old-hand)))
                          (post-pass "player->card-set")
                          (post-pass2 "player->card-set")))))
    (testing "Each player records shoot-moon strategy."
             (is (reduce #(and (boolean? %2) %)
                         true
                         (post-pass "shoot-moon"))))
    (testing "Players pass in different directions."
             (is (= [-1 4 1 2 3]
                    (mapv (fn [source-hand]
                            (reduce-kv (fn [acc key hand]
                                            (if (= 3 (count (intersection hand source-hand)))
                                              key acc))
                                       -1
                                       (post-pass "player->card-set")))
                          (pre-pass "player->card-set"))))
             (is (= [3 3 3 3]
                    (let [diff (partition 2 (interleave (pre-pass "card->player") (post-pass "card->player")))]
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
                                       (post-pass2 "player->card-set")))
                          (post-pass "player->card-set"))))
             (is (= [3 3 3 3]
                    (let [diff (partition 2 (interleave (post-pass "card->player") (post-pass2 "card->player")))]
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
                                       (post-pass3 "player->card-set")))
                          (post-pass2 "player->card-set"))))
             (is (= [3 3 3 3]
                    (let [diff (partition 2 (interleave (post-pass2 "card->player") (post-pass3 "card->player")))]
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
                                       (post-pass4 "player->card-set")))
                          (post-pass3 "player->card-set"))))
             (is (= [13 13 13 13]
                    (let [diff (partition 2 (interleave (post-pass3 "card->player") (post-pass4 "card->player")))]
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
                                       (post-pass5 "player->card-set")))
                          (post-pass4 "player->card-set"))))
             (is (= [3 3 3 3]
                    (let [diff (partition 2 (interleave (post-pass4 "card->player") (post-pass5 "card->player")))]
                      (vector (count (filter #(and (= 1 (first %)) (= 4 (second %))) diff))
                              (count (filter #(and (= 2 (first %)) (= 1 (second %))) diff))
                              (count (filter #(and (= 3 (first %)) (= 2 (second %))) diff))
                              (count (filter #(and (= 4 (first %)) (= 3 (second %))) diff)))))))
    (testing "Passed cards are recorded."
      (let [pre-pass
            {"turn-depth->(player->cumulative-points)" '([0 0 0 0 0] [0 0 0 0 0])
             "hand->round->player->card" '()
             "hand->round->player->play-order" '()
             "pass-direction" -1
             "human" [0 0 0 0 0]
             "player->card-set" [#{}
                             #{0 3 4 11 12 18 26 38 40 42 43 44 50}
                             #{2 9 14 16 17 20 25 27 28 29 34 48 49}
                             #{6 7 13 15 19 30 31 32 33 35 36 41 47}
                             #{1 5 8 10 21 22 23 24 37 39 45 46 51}]
             "card->player" [1 4 2 1 1 4 3 3 4 2 4 1 1 3 2 3 2 2 1 3 2
                             4 4 4 4 2 1 2 2 2 3 3 3 3 2 3 3 4 1 4 1 3
                             1 1 1 4 4 3 2 2 1 4]
             "passer->passee" {[0 0] '()}}
            post-pass (pass pre-pass)]
        (is (= (post-pass "passer->passee")
               {[0 0] '()
                [4 3] '(46 51 37)
                [3 2] '(6 7 47)
                [2 1] '(9 48 49)
                [1 4] '(0 50 38)}))))))

(deftest organize-new-hand-test
  (testing "Initialize information needed at the beginning of each hand."
    (let [pre-init (pass (create-game))
          post-init (organize-new-hand pre-init)
          post-init-static
          (organize-new-hand
           {"turn-depth->(player->cumulative-points)" '([0 0 0 0 0] [0 0 0 0 0])
            "hand->round->player->card" '()
            "hand->round->player->play-order" '()
            "pass-direction" 1
            "human" [0 0 0 0 0]
            "player->card-set" [#{}
                            #{0 2 4 9 14 16 21 25 27 31 37 49 51}
                            #{8 15 18 19 20 29 35 36 38 42 46 48 50}
                            #{1 11 12 13 23 24 32 33 34 39 41 43 45}
                            #{3 5 6 7 10 17 22 26 28 30 40 44 47}]
            "card->player" [1 3 1 4 1 4 4 4 2 1 4 3 3 3 1 2 1 4
                            2 2 2 1 4 3 3 1 4 1 4 2 4 1 3 3 3 2
                            2 1 2 3 4 3 2 3 4 3 2 4 2 1 2 1]
            "passer->passee" {}
            "shoot-moon" false})
          post-init-static-36 
          (organize-new-hand
           {"player->card-set"
            [#{}
             #{6 9 11 14 15 21 23 25 36 37 38 47 48}
             #{0 2 5 12 16 18 19 20 30 39 41 45 46}
             #{3 8 10 22 24 26 27 29 32 40 42 43 44}
             #{1 4 7 13 17 28 31 33 34 35 49 50 51}]
            "card->player"
            [2 4 2 3 4 2 1 4 3 1 3 1 2 4 1 1 2 4 2 2 2 1 3 1 3 1 3 3 4 3 2 4 3 4 4 4 1 1 1 2 3 2 3 3 3 2 2 1 1 4 4 4]
            "shoot-moon" [false false false false false]
            "pass-direction" 1
            "hand->round->player->card" '()
            "hand->round->player->play-order" '()
            "human" [0 0 0 0 0]
            "passer->passee" {[0 0] '(), [4 3] '(22 10 24), [3 2] '(45 46 12), [2 1] '(38 37 36), [1 4] '(49 50 51)}
            "turn-depth->(player->cumulative-points)" '([0 0 0 0 0] [0 0 0 0 0])})]
      (is (= #{"winning" "playable" "curr-player" "player->suit->count"
               "suits-known" "suit->player->broken" "player->player->could-have-36"}
             (difference (set (keys post-init)) (set (keys pre-init)))))
      (is (= [1 0] (post-init-static "winning")))
      (is (= 2 (post-init-static "curr-player")))
      ;dealer has 2 of clubs
      (is (= 0 (((post-init-static "player->card-set") 0) 0)))
      (is (= (post-init-static "suits-known")
             [[0 0 0 0] [4 4 3 2] [1 4 4 4] [3 3 3 4] [5 2 3 3]]))
      (is (= (post-init-static-36 "player->player->could-have-36")
            [[0 1 1 1 1] [0 2 1 1 1] [0 2 1 1 1] [0 1 1 1 1] [0 1 1 1 1]])))))

(deftest shoot-moon-test
  (testing "A hand with several unbroken sequences connected to high cards returns true."
           ;((8 9 10 11) (10 11 12) (9 10 11) (10 11 12))
           (is (shoot-moon (suitnum->handnum '((7 8 10 11)(9 10 12)(9 10 12)(7 8 9)))))
           ;((8 9 10 11) (9 10 12) (9 10 11 10 11 12))
           (is (not (shoot-moon (suitnum->handnum '((7 8 9 11)(9 10 12)(9 10 12)(7 8 9))))))))

(deftest moon-weights-test
  (testing "Cards that are isolated low have greater weight."
           (is (= (sort (moon-weights [1 11 12 13 23 24 32 33 34 39 41 43 45]))
                  '([0 11] [0 12] [1 23] [1 24] [4 32] [4 33] [4 34] [6 45] [7 43]
                       [8 41] [9 1] [9 39] [10 13])))))

(deftest commonly-high-test
  (testing "Cards over 7 are counted into their suits."
    ;((0 3 5 10 12) (8 11) (9) (4 6 7 8 9))
    (is (= (commonly-high (sort #{0 3 5 10 12 21 24 35 43 45 46 47 48}))
           [2 2 1 2]))))

(deftest unbroken-highest-test
  (testing "Number of continuously high cards in each suit with highest card in
           each suit is generated."
           (is (= (unbroken-highest (sort #{5 6 12 13 14 17 22 27 28 33 34 37 38}))
                  [[1 12] [1 9] [2 12] [0 -2]]))))

(deftest keep-queen-test
  (testing "Decide when to not pass queen of spades"
    (is (keep-queen 1 [0 0 0 0 0] 4 1 true))
    (is (not (keep-queen 1 [0 0 0 0 0] 4 -1 true)))
    (is (keep-queen 1 [0 0 21 0 0] 4 1 true))))

(deftest not-risky∵unplayed-test
  ;;completely rewrite
  (testing "Create weights for inverting card value given more known cards."
    (is (= (map #(format "%.2f" (float %))
                (let [hand (vec (suitnum->handnum '((0 1 2 4 6 7 9 12)(2 3 9 12)()(9))))]
                  (not-risky∵unplayed [8 4 0 1] hand)))
           '("12.62" "12.23" "11.85" "11.08" "10.31" "9.92" "9.15" "8.00" "13.69"
             "13.92" "15.31" "16.00" "19.92")))))

(deftest safe-from-36-test
  (testing "Able to know if impossible to get 36 (Q of Spades) based on
           yet to play players in hand->round->player->card and player->player->could-have-36"
           (is (safe-from-36? [-2 -1 -1 0 0] [0 0 0 2 0] 1))
           (is (not (safe-from-36? [-2 -1 -1 0 0] [0 0 2 0 0] 1)))))

(deftest probably-safe-from-36-test
  (testing "Able to know if unlikely to get 36 based on hand->round->player->card and
           player->player->could-have-36."
           (is (probably-safe-from-36? [-2 -1 -1 0 0] [0 0 0 1 1]))
           (is (not (probably-safe-from-36? [-2 -1 -1 0 0] [0 0 1 0 1])))))

(deftest safe-from-broken-test
  (testing "Able to know if unlikely to get 36 based on hand->round->player->card and
           player->player->could-have-36."
           (is (safe-from-broken? [-2 -1 -1 0 0] [0 0 0 1 1]))
           (is (not (safe-from-broken? [-2 -1 -1 0 0] [0 0 1 0 1])))))

#_(deftest safe-ish-test
  (testing "Able to know when less likely for others to break suit."
           (is (safe-ish 1 [[0 0 0 0][8 0 0 0][0 0 0 0][0 0 0 0][0 0 0 0]]
                         1 [-2 -1 -1 0] 0))
           (is (not (safe-ish 1 [[0 0 0 0][9 0 0 0][0 0 0 0][0 0 0 0][0 0 0 0]]
                              1 [-2 -1 -1 0] 0)))
           (is (not (safe-ish 5 [[0 0 0 0][8 0 0 0][0 0 0 0][0 0 0 0][0 0 0 0]]
                              1 [-2 -1 -1 0] 0)))
           (is (not (safe-ish 1 [[5 0 0 0][8 0 0 0][0 0 0 0][0 0 0 0][0 0 0 0]]
                              1 [-2 -1 -1 0] 0)))))

(deftest on-suit-weights-test
  (testing "Create weights for winning or losing a hand"
           (let [first-play
                 {"player->card-set"
                  [#{0}
                   #{6 9 11 14 15 21 23 25 36 37 38 47 48}
                   #{2 5 12 16 18 19 20 30 39 41 45 46}
                   #{3 8 10 22 24 26 27 29 32 40 42 43 44}
                   #{1 4 7 13 17 28 31 33 34 35 49 50 51}]
                  "card->player"
                  [0 4 2 3 4 2 1 4 3 1 3 1 2 4 1 1 2 4 2 2 2 1 3 1 3 1 3 3 4 3 2 4 3 4 4 4 1 1 1 2 3 2 3 3 3 2 2 1 1 4 4 4]
                  "suits-known" [[0 0 0 0] [3 5 3 2] [4 4 1 4] [3 2 4 4] [3 2 5 3]]
                  "shoot-moon" [false false false false false]
                  "player->player->could-have-36" [[0 1 1 1 1] [0 2 1 1 1] [0 2 1 1 1] [0 1 1 1 1] [0 1 1 1 1]]
                  "player->suit->count" [[0 0 0 0] [3 5 3 2] [4 4 1 4] [3 2 4 4] [3 2 5 3]]
                  "pass-direction" 1
                  "hand->round->player->card" '(([-2 -1 0 -1 -1]))
                  "winning" [2 0]
                  "playable" [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0]
                  "hand->round->player->play-order" '((2))
                  "human" [0 0 0 0 0]
                  "suit->player->broken" [[0 0 0 0 0] [0 0 0 0 0] [0 0 0 0 0] [0 0 0 0 0]]
                  "curr-player" 3
                  "passer->passee" {[0 0] '(), [4 3] '(22 10 24), [3 2] '(45 46 12), [2 1] '(38 37 36), [1 4] '(49 50 51)}
                  "turn-depth->(player->cumulative-points)" '([0 0 0 0 0] [0 0 0 0 0])}]
(is (= (on-suit-weights first-play) [3/100 2/25 1/10 0 0 0 0 0 0 0 0 0 0])))))

#_(deftest off-suit-weights-test
  (testing "Create weights for throwing a card."))

#_(deftest play-card-test
  (testing "Card is given to dealer. Capable of being called 3 times sequentially."
    (let [pre-play (organize-new-hand (pass (deal (create-game))))
          discard1 (play-card pre-play)
          discard2 (play-card discard1)
          discard3 (play-card discard2)]
      (is (= 1 (count ((pre-play "player->card-set") 0))))
      (is (= 2 (count ((discard1 "player->card-set") 0))))
      (is (= 3 (count ((discard2 "player->card-set") 0))))
      (is (= 4 (count ((discard3 "player->card-set") 0)))))
    #_(let [pre-play-static {"player->card-set" [#{0}
                                           #{1 2 3 6 7 9 16 17 25 37 38 42}
                                           #{5 10 18 19 20 23 31 32 39 40 46 50 51}
                                           #{12 13 14 15 27 28 30 36 41 43 44 48 49}
                                           #{4 8 11 21 22 24 26 29 33 34 35 45 47}],
                           "card->player" [0 1 1 1 4 2 1 1 4 1 2 4 3 3 3 3 1 1 2
                                           2 2 4 4 2 4 1 4 3 3 4 3 2 2 4 4 4 3 1
                                           1 2 2 3 1 3 3 4 2 4 3 3 2 2],
                           "pass-direction" 1,
                           "hand->round->player->card" '(([-1 0 -1 -1 -1])),
                           "winning" [1 0],
                           "playable" [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                                       1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 0
                                       0 0 0 0 0 0 0 0 0 0],
                           "hand->round->player->play-order" '((1)),
                           "human" [0 0 0 0 0],
                           "curr-player" 2,
                           "passer->passee" [],
                           "turn-depth->(player->cumulative-points)" '([0 0 0 0 0] [0 0 0 0 0] [0 0 0 0 0])}
          discard1 (play-card (choice-init pre-play-static))
          discard2 (play-card (choice-init discard1))
          discard3 (play-card (choice-init discard2))]
      (is (= #{0 11 10 12} ((discard3 "player->card-set") 0))))))
