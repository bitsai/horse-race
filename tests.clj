(ns tests
  (:use [engine]))

(let [names [:alice :bob :charlie]
      state {:moneys (init-moneys names)
             :cards (init-cards names)
             :horses (init-horses)
             :player-seq (cycle names)
             ;; :roll-seq (repeatedly roll-dice)
             :roll-seq (range 2 10)}]
  (print-states (get-scratch-history state)))
