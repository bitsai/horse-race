(ns engine
  (:use [clojure.contrib.generic.functor :only (fmap)])
  (:use [clojure.contrib.math :only (ceil)])
  (:use [util :only (count-if)]))

;; Game parameters
(def ranks (range 2 13))
(def deck (apply concat (repeat 4 ranks)))
(def finish-line (zipmap ranks [3 4 5 6 7 8 7 6 5 4 3]))

(defrecord horse-state [status position])
(defrecord game-state [moneys cards horses player-seq roll-seq log])

;; Init functions
(defn init-moneys [names]
  (-> (zipmap names (repeat 100)) (assoc :pot 0)))

;; re-think dealing logic? [Dale]
(defn init-cards [names]
  (let [cards-per-player (ceil (/ (count deck) (count names)))]
    (zipmap names (partition-all cards-per-player deck))))

(defn init-horses []
  (zipmap ranks (repeat (horse-state. :alive 0))))

(defn roll-dice []
  (+ (inc (rand-int 6)) (inc (rand-int 6))))

;; Utility functions
(defn pay [moneys player cost]
  (update-in moneys [player] - cost))

(defn pay-all [moneys card cost cards]
  (into {} (for [[player money] moneys]
             (let [cards-held (count-if #{card} (cards player))]
               [player (- money (* cost cards-held))]))))

(defn add-to-pot [moneys cost]
  (update-in moneys [:pot] + cost))

(defn discard-all [cards card]
  (fmap #(remove #{card} %) cards))

(defn scratch-horse [horses i position]
  (assoc horses i (horse-state. :scratched position)))

(defn advance-horse [horses i]
  (update-in horses [i :position] inc))

(defn count-scratched [horses]
  (count-if
   (fn [[_ {:keys [status]}]]
     (= status :scratched))
   horses))

(defn new-scratch-position [horses]
  (inc (count-scratched horses)))

(defn get-scratch-cost [position]
  (* position 5))

;; Game state predicates
(defn scratch-done? [{:keys [horses]}]
  (= (count-scratched horses) 4))

(defn race-done? [{:keys [horses]}]
  (some
   (fn [[i {:keys [status position]}]]
     (= position (finish-line i)))
   horses))

;; Game state-updating functions
(defn new-scratch [{:keys [moneys cards horses] :as state} roll]
  (let [position (new-scratch-position horses)
        cost (get-scratch-cost position)]
    (assoc state
      :moneys (-> moneys (pay-all roll cost cards) (add-to-pot (* cost 4)))
      :cards (-> cards (discard-all roll))
      :horses (-> horses (scratch-horse roll position)))))

(defn pay-scratch [{:keys [moneys player-seq] :as state} horse]
  (let [player (first player-seq)
        cost (get-scratch-cost (:position horse))]
    (assoc state
      :moneys (-> moneys (pay player cost) (add-to-pot cost)))))

(defn move-horse [{:keys [horses] :as state} roll]
  (assoc state
    :horses (-> horses (advance-horse roll))))

(defn next-turn [{:keys [player-seq roll-seq] :as state}]
  (assoc state
    :player-seq (rest player-seq)
    :roll-seq (rest roll-seq)))

(defn play-turn [{:keys [horses roll-seq] :as state}]
  (let [roll (first roll-seq)
        horse (horses roll)]
    (next-turn
     (cond (= (:status horse) :scratched) (-> state (pay-scratch horse))
           (scratch-done? state) (-> state (move-horse roll))
           :else (-> state (new-scratch roll))))))

;; History functions
(defn get-history [done? f state]
  (let [states (iterate f state)
        [not-dones [done]] (split-with (complement done?) states)]
    (concat not-dones [done])))

(defn get-race-history [state]
  (get-history race-done? play-turn state))

;; Output functions
(defn print-state [{:keys [moneys cards horses]}]
  (println "Moneys:" moneys)
  (println "Cards:" cards)
  (println "Horses:" horses)
  (newline))

(defn print-history [states]
  (doseq [s states]
    (print-state s)))
