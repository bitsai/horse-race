(ns game
  (:require [clojure.string :as str]
            [core :as core]))

(defn print-race
  [race]
  (doseq [{:keys [pot players horses]} race]
    (println "Pot:" pot)
    (println "Players:" players)
    (doseq [[n {:keys [position scratched?]}] horses]
      (let [dashes (repeat (core/finish-line n) \-)
            [before after] (split-at position dashes)]
        (println "Horse"
                 (format "%1$2s" n)
                 (if scratched?
                   (* 5 position)
                   (apply str (concat before ["X"] after))))))
    (newline)))

(defn get-names
  []
  (println "Enter player names:")
  (str/split (read-line) #"\s*,\s*"))

(defn get-chips
  []
  (println "Enter starting chips:")
  (Double/parseDouble (read-line)))

(defn play-game
  []
  (let [names (get-names)
        chips (get-chips)
        cards (shuffle core/deck)
        rolls (repeatedly core/roll-dice)]
    (loop [game (core/new-game names chips cards rolls)]
      (let [race (core/play-race game)]
        (print-race race)
        (println "Enter 'y' to play again:")
        (when (= "y" (read-line))
          (recur (core/reset-game (last race) (shuffle core/deck))))))))

(play-game)
