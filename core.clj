(ns core)

(def ranks (range 2 (inc 12)))
(def deck (apply concat (repeat 4 ranks)))
(def finish-line (zipmap ranks [3 4 5 6 7 8 7 6 5 4 3]))

(defrecord Player [name chips cards])
(defrecord Horse [position scratched?])
(defrecord Game [pot players horses rolls])

(defn roll-dice
  []
  (+ (inc (rand-int 6)) (inc (rand-int 6))))

;; Player functions
(defn dead?
  [player]
  (zero? (:chips player)))

(defn deal-cards
  [players cards]
  (let [n (count players)]
    (vec (map (fn [i p]
                (assoc p :cards (take-nth n (drop i cards))))
              (range)
              players))))

(defn count-cards
  [player rank]
  (count (filter #{rank} (:cards player))))

(defn discard-card
  [players rank]
  (vec (for [p players]
         (update-in p [:cards] #(remove #{rank} %)))))

(defn pay-cost
  [player cost]
  (let [chips (:chips player)
        payment (if (< chips cost) chips cost)
        new-chips (if (< chips cost) 0 (- chips cost))]
    [(assoc player :chips new-chips) payment]))

(defn pay-card
  [players rank cost]
  (let [player-payment (for [p players]
                         (let [p-cost (* cost (count-cards p rank))]
                           (pay-cost p p-cost)))
        paid-players (vec (map first player-payment))
        payment (apply + (map second player-payment))]
    [paid-players payment]))

(defn pay-share
  [player share]
  (if (dead? player)
    [player 0]
    [(update-in player [:chips] + share) share]))

(defn pay-pot
  [players winner-num pot]
  (let [share (/ pot 4)
        player-payment (for [p players]
                         (let [p-share (* share (count-cards p winner-num))]
                           (pay-share p p-share)))
        paid-players (vec (map first player-payment))
        payment (apply + (map second player-payment))]
    [paid-players (- pot payment)]))

;; Horse functions
(defn count-scratched
  [horses]
  (count (filter #(:scratched? %) (vals horses))))

(defn scratch-horse
  [horses n]
  (let [new-pos (inc (count-scratched horses))]
    (assoc horses n (Horse. new-pos true))))

(defn winner
  [horses]
  (first (filter (fn [[n horse]]
                   (and (not (:scratched? horse))
                        (= (:position horse) (finish-line n))))
                 horses)))

;; Game functions
(defn new-race
  [game cards]
  (assoc game
    :players (deal-cards (:players game) cards)
    :horses (zipmap ranks (repeat (Horse. 0 false)))))

(defn new-game
  [names chips cards rolls]
  (new-race (Game. 0
                   (map #(Player. % chips nil) names)
                   nil
                   rolls)
            cards))

(defn advance-player
  [game]
  (let [rotate-vec (fn [v] (conj (subvec v 1) (first v)))]
    (update-in game [:players] rotate-vec)))

(defn advance-turn
  [game]
  (advance-player (update-in game [:rolls] rest)))

(defn new-scratch
  [game]
  (let [{:keys [pot players horses rolls]} game
        new-pos (inc (count-scratched horses))
        cost (* 5 new-pos)
        roll (first rolls)
        [paid-players payment] (pay-card players roll cost)]
    (assoc game
      :pot (+ pot payment)
      :players (discard-card paid-players roll)
      :horses (scratch-horse horses roll))))

(defn pay-scratch
  [game]
  (let [{:keys [pot players horses rolls]} game
        cost (* 5 (:position (horses (first rolls))))
        [paid-player payment] (pay-cost (first players) cost)]
    (assoc game
      :pot (+ pot payment)
      :players (assoc players 0 paid-player))))

(defn move-horse
  [game]
  (let [roll (first (:rolls game))]
    (update-in game [:horses roll :position] inc)))

(defn play-turn
  [game]
  (let [{:keys [players horses rolls]} game
        player (first players)
        horse (horses (first rolls))]
    (cond (dead? player)                 (advance-player game)
          (:scratched? horse)            (advance-turn (pay-scratch game))
          (< (count-scratched horses) 4) (advance-turn (new-scratch game))
          :else                          (advance-turn (move-horse game)))))

(defn race-over?
  [game]
  (let [{:keys [players horses]} game]
    (or (every? dead? players)
        (winner horses))))

(defn split-pot
  [game]
  (let [{:keys [pot players horses]} game
        winner-num (key (winner horses))
        [paid-players remaining-pot] (pay-pot players winner-num pot)]
    (assoc game
      :pot remaining-pot
      :players paid-players)))

(defn play-race
  [game]
  (let [turns (iterate play-turn game)
        [race-turns [final-turn]] (split-with #(not (race-over? %)) turns)]
    (concat race-turns [final-turn] [(split-pot final-turn)])))
