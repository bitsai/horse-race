(ns tests
  (:require [core :as core])
  (:use [clojure.test]))

;; Player functions
(deftest deal-cards
  (is (= (core/deal-cards (repeat 3 {}) (range 10))
         [{:cards [0 3 6 9]}
          {:cards [1 4 7]}
          {:cards [2 5 8]}])))

(deftest pay-cost
  (is (= (core/pay-cost {:chips 10} 20)
         [{:chips 0} 10])))

(deftest pay-card
  (is (= (core/pay-card [{:chips 50 :cards [1 1]}
                         {:chips 30 :cards [1 2]}
                         {:chips 10 :cards [1 3]}]
                        1
                        20)
         [[{:chips 10 :cards [1 1]}
           {:chips 10 :cards [1 2]}
           {:chips  0 :cards [1 3]}]
          70])))

(deftest discard-card
  (is (= (core/discard-card [{:cards [0 0 0]}
                             {:cards [0 0 1]}
                             {:cards [0 1 1]}]
                            1)
         [{:cards [0 0 0]}
          {:cards [0 0]}
          {:cards [0]}])))

;; Horse functions
(deftest count-scratched
  (is (= (core/count-scratched {2 (core.Horse. 1 true)
                                3 (core.Horse. 2 true)
                                4 (core.Horse. 0 false)})
         2)))

(deftest scratch-horse
  (is (= (core/scratch-horse {2 (core.Horse. 1 true)
                              3 (core.Horse. 2 true)
                              4 (core.Horse. 0 false)}
                             4
                             3)
         {2 (core.Horse. 1 true)
          3 (core.Horse. 2 true)
          4 (core.Horse. 3 true)})))

(run-tests)
