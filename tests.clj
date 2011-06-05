(ns tests
  (:use [engine]))

(let [names [:alice :bob :charlie]
      rolls (repeatedly roll-dice)]
  (print-history (race-history (make-state names rolls))))
