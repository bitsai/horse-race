(ns core)

(def ranks (range 2 (inc 12)))
(def deck (apply concat (repeat 4 ranks)))
(def finish-line (zipmap ranks [3 4 5 6 7 8 7 6 5 4 3]))

(defrecord Player [name chips cards])
(defrecord Horse [position scratched?])
(defrecord Game [pot players horses rolls])

;; Player functions
(defn deal-cards
  [players cards]
  (let [n (count players)]
    (vec (map (fn [i p]
                (assoc p :cards (take-nth n (drop i cards))))
              (range)
              players))))

(defn pay-cost
  [player cost]
  (let [chips (:chips player)
        payment (if (< chips cost) chips cost)
        new-chips (if (< chips cost) 0 (- chips cost))]
    [(assoc player :chips new-chips) payment]))

(defn pay-card
  [players rank cost]
  (let [player-payment (for [p players]
                         (let [matches (filter #(= rank %) (:cards p))
                               total-cost (* cost (count matches))]
                           (pay-cost p total-cost)))
        paid-players (map first player-payment)
        payment (apply + (map second player-payment))]
    [(vec paid-players) payment]))

(defn discard-card
  [players rank]
  (vec (for [p players]
         (update-in p [:cards] #(remove #{rank} %)))))

;; Horse functions
(defn count-scratched
  [horses]
  (count (filter #(:scratched? %) (vals horses))))

(defn scratch-horse
  [horses number]
  (let [new-pos (inc (count-scratched horses))]
    (assoc horses number (Horse. new-pos true))))

;; Game functions
(defn new-game
  [names chips cards rolls]
  (Game. 0
         (deal-cards (map #(Player. % chips nil) names) cards)
         (zipmap ranks (repeat (Horse. 0 false)))
         rolls))

(defn reset-game
  [game cards]
  (assoc game
    :pot 0
    :players (deal-cards (:players game) cards)))

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

(defn advance-horse
  [game]
  (let [roll (first (:rolls game))]
    (update-in game [:horses roll :position] inc)))

(defn play-turn
  [game]
  (let [{:keys [players horses rolls]} game
        player (first players)
        horse (horses (first rolls))]
    (cond (zero? (:chips player))        (advance-player game)
          (:scratched? horse)            (advance-turn (pay-scratch game))
          (= 4 (count-scratched horses)) (advance-turn (advance-horse game))
          :else                          (advance-turn (new-scratch game)))))
