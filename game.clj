(ns game
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

(defn pay
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

(defn pay-scratch [[moneys cards horses [p & ps] [r & rs] cost-seq]]
  (let [cost (:scratched (horses r))]
    [(-> moneys (pay p cost) (add-to-pot cost))
     cards
     horses
     ps
     rs
     cost-seq]))

(defn new-scratch [[moneys cards horses [_ & ps] [r & rs] [c & cs]]]
  [(-> moneys (pay r c cards) (add-to-pot (* c 4)))
   (discard cards r)
   (scratch-horse horses r c)
   ps
   rs
   cs])

(defn scratch [[moneys cards horses players [r & _] cost-seq :as state]]
  (cond
   (empty? cost-seq) state
   (:scratched (horses r)) (pay-scratch state)
   :else (new-scratch state)))

(defn scratched? [[_ _ _ _ _ cost-seq]]
  (empty? cost-seq))

(defn get-history [condition f state]
  (let [states (iterate f state)
        [not-done [done & _]] (split-with (complement condition) states)]
    (concat not-done [done])))

;; Player names
(def names [:alice :bob :charlie])

;; Game state
(def moneys (-> (zipmap names (repeat 100)) (assoc :pot 0)))
(def cards (deal deck names))
(def horses (zipmap ranks (repeat {:position 0})))
(def player-seq (cycle names))
;;(def roll-seq (repeatedly roll-dice))
(def roll-seq [2 3 4 5])
(def cost-seq [5 10 15 20])

(defn print-state [[moneys cards horses _ _ _]]
  (println "Moneys:" moneys)
  (println "Cards:" cards)
  (println "Horses:" horses)
  (newline))

(defn print-states [states]
  (doseq [s states]
    (print-state s)))

;; Test generating scratch history
(let [state [moneys cards horses player-seq roll-seq cost-seq]]
  (print-states (get-history scratched? scratch state)))
