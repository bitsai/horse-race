(ns engine
  (:use [clojure.contrib.generic.functor :only (fmap)])
  (:use [clojure.contrib.math :only (ceil)])
  (:use [util :only (any? count-if)]))

;; Game parameters
(def ranks (range 2 13))
(def deck (apply concat (repeat 4 ranks)))
(def finish-line (zipmap ranks [3 4 5 6 7 8 7 6 5 4 3]))

;; Init functions
(defn init-moneys [names]
  (-> (zipmap names (repeat 100)) (assoc :pot 0)))

;; re-think dealing logic? [Dale]
(defn init-cards [names]
  (let [cards-per-player (ceil (/ (count deck) (count names)))]
    (zipmap names (partition-all cards-per-player deck))))

(defn init-horses []
  (zipmap ranks (repeat {:position 0})))

(defn roll-dice []
  (+ (inc (rand-int 6)) (inc (rand-int 6))))

;; Utility functions
(defn deduct
  ([moneys player amount]
     (update-in moneys [player] - amount))
  ([moneys cards roll amount]
     (into {} (for [[player money] moneys]
                (let [cards-held (count-if #{roll} (player cards))]
                  [player (- money (* amount cards-held))])))))

(defn add-to-pot [moneys amount]
  (update-in moneys [:pot] + amount))

(defn discard [cards roll]
  (fmap #(remove #{roll} %) cards))

(defn scratch-horse [horses roll amount]
  (assoc horses roll {:scratched amount}))

(defn advance-horse [horses roll]
  (update-in horses [roll :position] inc))

(defn count-scratched-horses [horses]
  (count-if
   (fn [[_ horse-state]]
     (:scratched horse-state))
   horses))

(defn new-scratch-cost [horses]
  (* 5 (inc (count-scratched-horses horses))))

;; State predicates
(defn scratch-done? [{:keys [horses]}]
  (= 4 (count-scratched-horses horses)))

(defn race-done? [{:keys [horses]}]
  (any?
   (fn [[horse-num {:keys [position]}]]
     (= position (finish-line horse-num)))
   horses))

;; State-updating functions
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

(defn new-scratch [{:keys [moneys cards horses roll-seq] :as state}]
  (let [roll (first roll-seq)
        cost (new-scratch-cost horses)]
    (assoc state
      :moneys (-> moneys (deduct cards roll cost) (add-to-pot (* cost 4)))
      :cards (-> cards (discard roll))
      :horses (-> horses (scratch-horse roll cost)))))

(defn move-horse [{:keys [horses roll-seq] :as state}]
  (let [roll (first roll-seq)]
    (assoc state
      :horses (-> horses (advance-horse roll)))))

(defn play-turn [{:keys [horses roll-seq] :as state}]
  (let [roll (first roll-seq)
        horse (horses roll)]
    (next-turn (cond
                (:scratched horse) (pay-scratch state)
                (scratch-done? state) (move-horse state)
                :else (new-scratch state)))))

;; History functions
(defn get-history [done? f state]
  (let [states (iterate f state)
        [not-dones [done]] (split-with (complement done?) states)]
    (concat not-dones [done])))

(defn get-scratch-history [state]
  (get-history scratch-done? play-turn state))

(defn get-race-history [state]
  (get-history race-done? play-turn state))

;; Output functions
(defn print-state [{:keys [moneys cards horses]}]
  (println "Moneys:" moneys)
  (println "Cards:" cards)
  (println "Horses:" horses)
  (newline))

(defn print-states [states]
  (doseq [s states]
    (print-state s)))
