(ns tests
  (:use [engine]))

(let [names ["Charlie" "Bob" "Alice"]
      cards (shuffle deck)
      rolls (repeatedly roll-dice)
      history (race-history (make-state names cards rolls))]
  (print-history history)
  (print-end-game (last history)))
