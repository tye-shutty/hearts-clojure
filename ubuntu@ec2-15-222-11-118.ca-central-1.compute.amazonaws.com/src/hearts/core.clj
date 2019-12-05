(ns hearts.core)

;;most of this can be put in a map and passed between functions, but atoms are good for troubleshooting and piecemeal development
(def player-cards "player/dealer pos assoc with cards in sorted-set (dealer is 0, other decks start at 52) [#{8 15 42 46 51} #{1 7 33 43}]. aces high" (atom []))
(def card-players "card pos assoc with player/dealer num [1 4]" (atom []))
(def player-suits-broken "player pos then suit pos (y/n=1/0 for that suit) [[1 1 1 1] [0 1 1 0]]" (atom []))
(def suit-players-broken "suit pos then player pos (y/n = 1/0) [[0 0 0 0] [0 1 1 0]]" (atom []))
(def curr-winner (atom [-1 -1]))
(def game-points "hand pos (most recent to oldest) then player pos '([13 4 0 9] [0 13 13 0])" (atom '())) ;could save points of every hand
(def hand-points "player pos [0 0 0 1]" (atom []))
(def round-history "most recent hand first, most recent round first, sorted maps ordered by play, key is player, val is card '('({1 6 2 8 3 51 0 12}))" (atom '()))
(def player-history "no purpose yet, doesn't have play order, just ordered by player then by round [[9 22] [5 16] [12 47] [0 13]]")
(def human "player pos y/n [0 0 0 1]" (atom []))
(def pass-direction "-1=right, 1=left(clockwise, the order of the player-cards), 2=across(for now), 0=no passing" (atom -1))

(defn deal [numplayers] "todo: expand beyond 1 deck, 4 players"
  (loop [players (vec (take (inc numplayers) (repeat (sorted-set))))
         cards (vec (take 52 (repeat 0)))
         loose-cards (range 52)
         needy-players (vec (partition 2 (interleave (range 1 (inc numplayers)) (repeat 0))))]
    (if (< (count loose-cards) 1)
      (if (> (count needy-players) 0)
        (throw (Exception. "some players still need cards"))
        (do (reset! player-cards players)
          (reset! card-players cards)))
      (let [chosen-index (rand-int (count needy-players))
            [chosen chosen-count] (needy-players chosen-index)]
        (recur (update players chosen conj (first loose-cards))
               (update cards (first loose-cards) (constantly chosen))
               (rest loose-cards)
               (do (prn needy-players)(prn loose-cards)
                 (if (> chosen-count 11)
                   (vec (keep-indexed #(if (= chosen-index %) nil %2) needy-players))
                   (update needy-players chosen-index #(list (first %) (inc (second %))))))))))
  (reset! player-suits-broken (vec (take numplayers (repeat (vec (take numplayers (repeat 0)))))))
  (reset! suit-players-broken (vec (take numplayers (repeat (vec (take numplayers (repeat 0)))))))
  (reset! hand-points (vec (take numplayers (repeat 0))))
  (swap! game-points conj (vec (take numplayers (repeat 0)))) ;needs to be reset before a game
  (reset! curr-winner [-1 -1])
  (reset! pass-direction -1))

(defn move-card [card from to]
  (prn "from" from "to" to "card" card)
  (when (= from to) (throw (Exception. "a player is passing to itself")))
  (swap! player-cards
         (fn [y] (let [z (update y from (fn [x] (disj x card)))]
                   (if (y card) nil (throw (Exception. "card not found")))
                   (update z to conj card))))
  (swap! card-players update card (constantly to)))

(defn track-card-pass [suit card from to] ;suits = 0 1 2 3 ;also need to know about suits played and not broken, as well as high cards played
  "tracks if suit is broken"
  (if (= suit (int (/ card 13)))
      nil
      (do (swap! player-suits-broken update from update suit (constantly 1))
        (swap! suit-players-broken update suit update from (constantly 1))))
  (move-card card from to))

(defn human-pass [human-player target-player]
  (prn (str "Player " human-player ", what card of yours will you pass to player " target-player "?\n" "Your cards: " (@player-cards human-player)))
  (let [card (clojure.edn/read-string (read-line))] (if (= (type card) Long) (move-card card human-player target-player) (throw (Exception. "not a java.lang.Long")))))

;not based on >1 decks (yes it is b/c new decks will start at 52, all cards will have an unique #, test for = with mod 52) ;no, bad for the card-players vec ;no it's ok
;out of date
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

(defn pass []
  (if (= @pass-direction 0) nil
    (doseq [player (range 1 (count @player-cards))]
     (let [destination (if (= (+ x 1) (count @player-cards)) 1 (+ player 1))]
       (if (= (@human player) 1)
         (human-select-pass player)
         (let [hand (@player-cards player)
               suit-nums (reduce #(update %1 (int (/ %2 13)) inc) [0 0 0 0] hand) ;num cards in each suit
               suit-not-nums (map #(- % 13) suit-nums)
               score (@game-points 0)
               special-card-weights {36 #_queen-spades (if (reduce #(or %1 (> (score %2) (+ 20 (score player)))) false score) 1 90) ;keep queen if behind
                                     37 #_king-spades 100
                                     38 #_ace-spades 100
                                     0 #_two-clubs 20}
               suit-weights [1 1 1 2]
               weights (map (fn [card] (* (suit-weights (int (/ card 13)))
                                          (card special-card-weights card) ;hand weight is the value of the card if no special weight
                                          (suit-not-nums (int (/ card 13)))))
                            hand)]
           (doseq [card (take 3 (sort > hand))]
             (track-card-pass (int (/ card 13)) card player destination))))))))

(defn human-select-pass [])

(defn all-play1-round1 []
  (doseq [player-cards (let [first-player (rand-nth (@card-players 0))]
                         (concat (take-last (- (count @player-cards) first-player) @player-cards)
                                 (rest (take first-player @player-cards))))]
    (prn player-cards)
    (track-card-pass 1
      (let [card (or (if (empty? (filter #(= (first player-cards) %) (@card-players 0))) nil 0)
                     (let [clubs (filter #(< % 13) (second player-cards))] (if (empty? clubs) nil (rand-nth clubs)))
                     (let [nonpoint (filter #(or (< % 37) (= % 38)) (second player-cards))] (if (empty? nonpoint) nil (rand-nth nonpoint)))
                     (throw (Exception. "player has no card to play in the first round")))]
        (if (and (< card 13) (> card (second @curr-winner))) (reset! curr-winner [(first player-cards) card])) card)
      (first player-cards) 0)))

(defn all-play-another-round [])


(defn -main
  [& args]
  (let [name (if (empty? args)
               (do (println "What is your name?") (read-line))
               (first args))]
    (println (str "hi " name))))
tom
