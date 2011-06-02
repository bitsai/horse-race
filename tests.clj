(ns tests
  (:use [engine]))

(let [names [:alice :bob :charlie]
      state (engine.game-state. (init-moneys names)
                                (init-cards names)
                                (init-horses)
                                (cycle names)
                                ;; (repeatedly roll-dice)
                                [2 3 4 5]
                                [])]
  (print-states (get-scratch-history state)))
