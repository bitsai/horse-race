(ns engine
  (:require [clojure.contrib.string :as str])
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

(defn all-pay [moneys card cost cards]
  (into {} (for [[player money] moneys]
             (let [cards-held (count-if #{card} (cards player))]
               [player (- money (* cost cards-held))]))))

(defn add-to-pot [moneys cost]
  (update-in moneys [:pot] + cost))

(defn discard [cards card]
  (fmap #(remove #{card} %) cards))

(defn scratch-horse [horses i position]
  (assoc horses i (horse-state. :scratched position)))

(defn advance-horse [horses i]
  (update-in horses [i :position] inc))

(defn scratched-horse? [horse]
  (= (:status horse) :scratched))

(defn count-scratched [horses]
  (count-if (fn [[_ horse]] (scratched-horse? horse)) horses))

(defn new-scratch-position [horses]
  (inc (count-scratched horses)))

(defn scratch-cost [position]
  (* position 5))

;; Game state predicates
(defn scratched-state? [{:keys [horses]}]
  (= (count-scratched horses) 4))

(defn race-finished? [{:keys [horses]}]
  (some
   (fn [[i {:keys [status position]}]]
     (and (= status :alive)
          (= position (finish-line i))))
   horses))

;; Game state-updating functions
(defn new-scratch [{:keys [moneys cards horses] :as state} roll]
  (let [position (new-scratch-position horses)
        cost (scratch-cost position)]
    (assoc state
      :moneys (-> moneys (all-pay roll cost cards) (add-to-pot (* cost 4)))
      :cards (-> cards (discard roll))
      :horses (-> horses (scratch-horse roll position)))))

(defn pay-scratch [{:keys [moneys player-seq] :as state} horse]
  (let [player (first player-seq)
        cost (scratch-cost (:position horse))]
    (assoc state
      :moneys (-> moneys (pay player cost) (add-to-pot cost)))))

(defn move-horse [{:keys [horses] :as state} roll]
  (assoc state
    :horses (-> horses (advance-horse roll))))

(defn advance-turn [{:keys [player-seq roll-seq] :as state}]
  (assoc state
    :player-seq (rest player-seq)
    :roll-seq (rest roll-seq)))

(defn play-turn [{:keys [horses roll-seq] :as state}]
  (let [roll (first roll-seq)
        horse (horses roll)]
    (advance-turn
     (cond (scratched-horse? horse) (-> state (pay-scratch horse))
           (scratched-state? state) (-> state (move-horse roll))
           :else (-> state (new-scratch roll))))))

;; History functions
(defn history [pred f state]
  (let [states (iterate f state)
        [unfinished [finished]] (split-with (complement pred) states)]
    (concat unfinished [finished])))

(defn race-history [state]
  (history race-finished? play-turn state))

;; Output functions
(defn print-state [{:keys [moneys cards horses]}]
  (println "Moneys:" moneys)
  (println "Cards:" cards)
  (doseq [[i {:keys [status position]}] horses]
    (println "Horse"
             (format "%1$2s" i)
             (if (= status :scratched)
               (scratch-cost position)
               (str (str/repeat position "-")
                    "O"
                    (str/repeat (- (finish-line i) position) "-")))))
  (newline))

(defn print-history [states]
  (doseq [s states] (print-state s)))
