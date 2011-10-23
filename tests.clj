(ns tests
  (:require [core :as core])
  (:use [clojure.test]))

;; Player functions
(deftest deal-cards
  (is (= (core/deal-cards [(core.Player. "A" nil nil)
                           (core.Player. "B" nil nil)
                           (core.Player. "C" nil nil)]
                          (range 10))
         [(core.Player. "A" nil [0 3 6 9])
          (core.Player. "B" nil [1 4 7])
          (core.Player. "C" nil [2 5 8])])))

(deftest pay-cost
  (is (= (core/pay-cost (core.Player. "A" 10 nil) 20)
         [(core.Player. "A" 0 nil) 10])))

(deftest pay-card
  (is (= (core/pay-card [(core.Player. "A" 50 [1 1])
                         (core.Player. "B" 30 [1 2])
                         (core.Player. "C" 10 [1 3])]
                        1
                        20)
         [[(core.Player. "A" 10 [1 1])
           (core.Player. "B" 10 [1 2])
           (core.Player. "C"  0 [1 3])]
          70])))

(deftest discard-card
  (is (= (core/discard-card [(core.Player. "A" nil [0 0 0])
                             (core.Player. "B" nil [0 0 1])
                             (core.Player. "C" nil [0 1 1])]
                            1)
         [(core.Player. "A" nil [0 0 0])
          (core.Player. "B" nil [0 0])
          (core.Player. "C" nil [0])])))

;; Horse functions
(deftest count-scratched
  (is (= (core/count-scratched {2 (core.Horse. nil false)
                                3 (core.Horse. nil true)
                                4 (core.Horse. nil true)})
         2)))

(deftest scratch-horse
  (is (= (core/scratch-horse {2 (core.Horse. nil nil)
                              3 (core.Horse. nil nil)
                              4 (core.Horse. nil nil)}
                             2
                             1)
         {2 (core.Horse. 1 true)
          3 (core.Horse. nil nil)
          4 (core.Horse. nil nil)})))

;; Game functions
(deftest new-game
  (is (= (core/new-game ["A" "B" "C"] 20 (range 10) [0])
         (core.Game. 0
                     [(core.Player. "A" 20 [0 3 6 9])
                      (core.Player. "B" 20 [1 4 7])
                      (core.Player. "C" 20 [2 5 8])]
                     {2 (core.Horse. 0 false)
                      3 (core.Horse. 0 false)
                      4 (core.Horse. 0 false)
                      5 (core.Horse. 0 false)
                      6 (core.Horse. 0 false)
                      7 (core.Horse. 0 false)
                      8 (core.Horse. 0 false)
                      9 (core.Horse. 0 false)
                      10 (core.Horse. 0 false)
                      11 (core.Horse. 0 false)
                      12 (core.Horse. 0 false)}
                     [0]))))

(deftest reset-game
  (is (= (core/reset-game (core.Game. 10
                                      [(core.Player. "A" nil nil)
                                       (core.Player. "B" nil nil)
                                       (core.Player. "C" nil nil)]
                                      nil
                                      nil)
                          (range 10))
         (core.Game. 0
                     [(core.Player. "A" nil [0 3 6 9])
                      (core.Player. "B" nil [1 4 7])
                      (core.Player. "C" nil [2 5 8])]
                     nil
                     nil))))

(deftest advance-horse
  (is (= (core/advance-horse (core.Game. nil
                                         nil
                                         {2 (core.Horse. 0 false)
                                          3 (core.Horse. 0 false)
                                          4 (core.Horse. 0 false)}
                                         [2]))
         (core.Game. nil
                     nil
                     {2 (core.Horse. 1 false)
                      3 (core.Horse. 0 false)
                      4 (core.Horse. 0 false)}
                     [2]))))

(deftest advance-player
  (is (= (core/advance-player (core.Game. nil
                                          [(core.Player. "A" nil nil)
                                           (core.Player. "B" nil nil)
                                           (core.Player. "C" nil nil)]
                                          nil
                                          nil))
         (core.Game. nil
                     [(core.Player. "B" nil nil)
                      (core.Player. "C" nil nil)
                      (core.Player. "A" nil nil)]
                     nil
                     nil))))

(deftest advance-turn
  (is (= (core/advance-turn (core.Game. nil
                                        [(core.Player. "A" nil nil)
                                         (core.Player. "B" nil nil)
                                         (core.Player. "C" nil nil)]
                                        nil
                                        [0]))
         (core.Game. nil
                     [(core.Player. "B" nil nil)
                      (core.Player. "C" nil nil)
                      (core.Player. "A" nil nil)]
                     nil
                     []))))

(run-tests)
