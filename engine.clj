(ns engine
  (:use [util :only (count-if find-first)]))

;; Game parameters
(def ranks (range 2 13))
(def deck (apply concat (repeat 4 ranks)))
(def finish-line (zipmap ranks [3 4 5 6 7 8 7 6 5 4 3]))

;; State structures
(defrecord HorseState [number status position finish])
(defrecord GameState [chips cards horses player-seq roll-seq log])

;; Init functions
(defn init-chips [names]
  (-> (zipmap names (repeat 100)) (assoc :pot 0)))

(defn init-cards [names cards]
  (let [n (java.lang.Math/ceil (/ (count cards) (count names)))]
    (zipmap names (partition-all n cards))))

(defn init-horses []
  (into {} (for [i ranks]
             [i (HorseState. i :alive 0 (finish-line i))])))

(defn roll-dice []
  (+ (inc (rand-int 6)) (inc (rand-int 6))))

;; Misc functions
(defn scratch-cost [position]
  (* position 5))

;; Horse state predicates
(defn scratched-horse? [{:keys [status]}]
  (= status :scratched))

(defn winning-horse? [{:keys [status position finish]}]
  (and (= status :alive)
       (= position finish)))

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
  (into {} (for [[player player-cards] cards]
             [player (remove #{card} player-cards)])))

;; Horses functions
(defn scratch-horse [horses i position]
  (assoc horses i (HorseState. i :scratched position nil)))

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
  (some winning-horse? (vals horses)))

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
  (doseq [{:keys [number status position finish]} (vals horses)]
    (println "Horse"
             (format "%1$2s" number)
             (if (= status :scratched)
               (scratch-cost position)
               (str (apply str (repeat position \-))
                    "O"
                    (apply str (repeat (- finish position) \-))))))
  (newline))

(defn print-history [states]
  (doseq [s states] (print-state s)))

(defn print-end-game [{:keys [chips cards horses]}]
  (let [winner (:number (find-first winning-horse? (vals horses)))
        chips-per-share (/ (chips :pot) 4.0)]
    (println "Horse" winner "won!")
    (doseq [[player player-cards] cards]
      (let [shares (count-if #{winner} player-cards)
            winnings (* chips-per-share shares)]
        (println player "won" winnings ";"
                 "final total =" (+ winnings (chips player)))))))
