(ns engine
  (:require [clojure.contrib.string :as str])
  (:use [clojure.contrib.generic.functor :only (fmap)])
  (:use [clojure.contrib.math :only (ceil)])
  (:use [util :only (count-if)]))

;; Game parameters
(def ranks (range 2 13))
(def deck (apply concat (repeat 4 ranks)))
(def finish-line (zipmap ranks [3 4 5 6 7 8 7 6 5 4 3]))

;; State structures
(defrecord HorseState [status position])
(defrecord GameState [chips cards horses player-seq roll-seq log])

;; Init functions
(defn init-chips [names]
  (-> (zipmap names (repeat 100)) (assoc :pot 0)))

;; Discuss this logic with Dale
(defn init-cards [names cards]
  (let [cards-per-player (ceil (/ (count cards) (count names)))]
    (zipmap names (partition-all cards-per-player cards))))

(defn init-horses []
  (zipmap ranks (repeat (HorseState. :alive 0))))

(defn roll-dice []
  (+ (inc (rand-int 6)) (inc (rand-int 6))))

;; Misc functions
(defn scratched-horse? [{:keys [status]}]
  (= status :scratched))

(defn scratch-cost [position]
  (* position 5))

;; Chips functions
(defn pay [chips player cost]
  (update-in chips [player] - cost))

(defn all-pay [chips card cost cards]
  (into {} (for [[player player-chips] chips]
             (let [cards-held (count-if #{card} (cards player))]
               [player (- player-chips (* cost cards-held))]))))

(defn add-to-pot [chips cost]
  (update-in chips [:pot] + cost))

;; Cards functions
(defn all-discard [cards card]
  (fmap #(remove #{card} %) cards))

;; Horses functions
(defn scratch-horse [horses i position]
  (assoc horses i (HorseState. :scratched position)))

(defn advance-horse [horses i]
  (update-in horses [i :position] inc))

(defn count-scratched [horses]
  (count-if scratched-horse? (vals horses)))

(defn new-scratch-position [horses]
  (inc (count-scratched horses)))

;; Game state predicates
(defn scratch-finished? [{:keys [horses]}]
  (= (count-scratched horses) 4))

(defn race-finished? [{:keys [horses]}]
  (some
   (fn [[i {:keys [status position]}]]
     (and (= status :alive)
          (= position (finish-line i))))
   horses))

;; Game state functions
(defn show-player-roll [{:keys [player-seq roll-seq]}]
  (let [player (first player-seq)
        roll (first roll-seq)]
    (str player " rolled a " roll)))

(defn new-scratch [{:keys [chips cards horses] :as state} roll]
  (let [position (new-scratch-position horses)
        cost (scratch-cost position)]
    (assoc state
      :chips (-> chips (all-pay roll cost cards) (add-to-pot (* cost 4)))
      :cards (-> cards (all-discard roll))
      :horses (-> horses (scratch-horse roll position))
      :log (str (show-player-roll state) "; "
                "horse " roll " got scratched, "
                "everyone paid " cost "/card"))))

(defn pay-scratch [{:keys [chips horses player-seq] :as state} roll]
  (let [player (first player-seq)
        cost (scratch-cost (:position (horses roll)))]
    (assoc state
      :chips (-> chips (pay player cost) (add-to-pot cost))
      :log (str (show-player-roll state) "; "
                "horse " roll " already scratched, "
                player " paid " cost))))

(defn move-horse [{:keys [horses] :as state} roll]
  (assoc state
    :horses (-> horses (advance-horse roll))
    :log (str (show-player-roll state) "; "
              "horse " roll " advanced")))

(defn advance-turn [{:keys [player-seq roll-seq] :as state}]
  (assoc state
    :player-seq (rest player-seq)
    :roll-seq (rest roll-seq)))

(defn play-turn [{:keys [horses roll-seq] :as state}]
  (let [roll (first roll-seq)]
    (advance-turn
     (cond (scratched-horse? (horses roll)) (-> state (pay-scratch roll))
           (scratch-finished? state) (-> state (move-horse roll))
           :else (-> state (new-scratch roll))))))

(defn make-state [names cards roll-seq]
  (GameState. (init-chips names)
              (init-cards names cards)
              (init-horses)
              (cycle names)
              roll-seq
              "New race started"))

;; History functions
(defn history [pred f state]
  (let [states (iterate f state)
        [unfinished [finished]] (split-with (complement pred) states)]
    (concat unfinished [finished])))

(defn race-history [state]
  (history race-finished? play-turn state))

;; Output functions
(defn print-state [{:keys [chips cards horses log]}]
  (println "Log:" log)
  (println "Chips:" chips)
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
