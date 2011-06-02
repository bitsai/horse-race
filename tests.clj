(ns tests
  (:use [engine]))

(let [names [:alice :bob :charlie]
      state (engine.game-state. (init-moneys names)
                                (init-cards names)
                                (init-horses)
                                (cycle names)
                                (repeatedly roll-dice)
                                [])]
  (print-history (get-race-history state)))
