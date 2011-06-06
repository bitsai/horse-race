(ns tests
  (:use [engine]))

(let [names ["Alice" "Bob" "Charlie"]
      cards (shuffle deck)
      rolls (repeatedly roll-dice)]
  (print-history (race-history (make-state names cards rolls))))
