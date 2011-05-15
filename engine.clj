(ns engine
  (:use [clojure.contrib.generic.functor :only (fmap)])
  (:use [clojure.contrib.math :only (ceil)])
  (:use [util :only (count-if)]))

;; Game parameters
(def ranks (range 2 13))
(def deck (apply concat (repeat 4 ranks)))
(def finish-line (zipmap ranks [3 4 5 6 7 8 7 6 5 4 3]))

;; Game functions
(defn deal [deck names]
  (let [hand-size (ceil (/ (count deck) (count names)))
        hands (partition-all hand-size deck)]
    (zipmap names hands)))

(defn roll-dice []
  (+ (inc (rand-int 6)) (inc (rand-int 6))))

(defn deduct
  ([moneys player amount]
     (update-in moneys [player] - amount))
  ([moneys roll amount cards]
     (into {} (for [[player money] moneys]
                (let [cards-held (count-if #{roll} (player cards))]
                  [player (- money (* amount cards-held))])))))

(defn add-to-pot [moneys amount]
  (update-in moneys [:pot] + amount))

(defn discard [cards roll]
  (fmap #(remove #{roll} %) cards))

(defn scratch-horse [horses roll amount]
  (assoc horses roll {:scratched amount}))

(defn next-turn [{:keys [player-seq roll-seq] :as state}]
  (assoc state
    :player-seq (rest player-seq)
    :roll-seq (rest roll-seq)))

(defn pay-scratch [{:keys [moneys horses player-seq roll-seq] :as state}]
  (let [player (first player-seq)
        roll (first roll-seq)
        cost (:scratched (horses roll))]
    (assoc state
      :moneys (-> moneys (deduct player cost) (add-to-pot cost)))))

(defn new-scratch [{:keys [moneys cards horses roll-seq costs] :as state}]
  (let [roll (first roll-seq)
        cost (first costs)]
    (assoc state
      :moneys (-> moneys (deduct roll cost cards) (add-to-pot (* cost 4)))
      :cards (-> cards (discard roll))
      :horses (-> horses (scratch-horse roll cost))
      :costs (rest costs))))

(defn scratched? [{:keys [costs]}]
  (empty? costs))

(defn scratch [{:keys [horses roll-seq] :as state}]
  (let [roll (first roll-seq)
        horse (horses roll)]
    (next-turn (cond
                (scratched? state) state
                (:scratched horse) (pay-scratch state)
                :else (new-scratch state)))))

(defn get-history [condition f state]
  (let [states (iterate f state)
        [not-dones [done & _]] (split-with (complement condition) states)]
    (concat not-dones [done])))

;; Player names
(def names [:alice :bob :charlie])

;; Game state
(def moneys (-> (zipmap names (repeat 100)) (assoc :pot 0)))
(def cards (deal deck names))
(def horses (zipmap ranks (repeat {:position 0})))
(def player-seq (cycle names))
;;(def roll-seq (repeatedly roll-dice))
(def roll-seq [2 3 4 5])
(def costs [5 10 15 20])

(defn print-state [{:keys [moneys cards horses]}]
  (println "Moneys:" moneys)
  (println "Cards:" cards)
  (println "Horses:" horses)
  (newline))

(defn print-states [states]
  (doseq [s states]
    (print-state s)))

(defn advance-horse [horses x]
  (update-in horses [x :position] inc))

;; Test generating scratch history
(let [state {:moneys moneys :cards cards :horses horses
             :player-seq player-seq :roll-seq roll-seq :costs costs}]
  (print-states (get-history scratched? scratch state)))
