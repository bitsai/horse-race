(ns tests
  (:require [core :as core])
  (:use [clojure.test]))

;; Player functions
(deftest deal-cards
  (is (= (core/deal-cards [(core.Player. "A" 0 [])
                           (core.Player. "B" 0 [])
                           (core.Player. "C" 0 [])]
                          (range 2 12))
         [(core.Player. "A" 0 [2 5 8 11])
          (core.Player. "B" 0 [3 6 9])
          (core.Player. "C" 0 [4 7 10])])))

(deftest pay-cost
  (is (= (core/pay-cost (core.Player. "A" 10 []) 20)
         [(core.Player. "A" 0 []) 10])))

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
  (is (= (core/discard-card [(core.Player. "A" 0 [0 0 0])
                             (core.Player. "B" 0 [0 0 1])
                             (core.Player. "C" 0 [0 1 1])]
                            1)
         [(core.Player. "A" 0 [0 0 0])
          (core.Player. "B" 0 [0 0])
          (core.Player. "C" 0 [0])])))

;; Horse functions
(deftest count-scratched
  (is (= (core/count-scratched {2 (core.Horse. 0 false)
                                3 (core.Horse. 1 true)
                                4 (core.Horse. 2 true)})
         2)))

(deftest scratch-horse
  (is (= (core/scratch-horse {2 (core.Horse. 0 false)
                              3 (core.Horse. 1 true)
                              4 (core.Horse. 2 true)}
                             2)
         {2 (core.Horse. 3 true)
          3 (core.Horse. 1 true)
          4 (core.Horse. 2 true)})))

;; Game functions
(deftest new-game
  (is (= (core/new-game ["A" "B" "C"] 20 (range 2 12) [2 3 4])
         (core.Game. 0
                     [(core.Player. "A" 20 [2 5 8 11])
                      (core.Player. "B" 20 [3 6 9])
                      (core.Player. "C" 20 [4 7 10])]
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
                     [2 3 4]))))

(deftest reset-game
  (is (= (core/reset-game (core.Game. 10
                                      [(core.Player. "A" 0 [])
                                       (core.Player. "B" 0 [])
                                       (core.Player. "C" 0 [])]
                                      {}
                                      [])
                          (range 2 12))
         (core.Game. 0
                     [(core.Player. "A" 0 [2 5 8 11])
                      (core.Player. "B" 0 [3 6 9])
                      (core.Player. "C" 0 [4 7 10])]
                     {}
                     []))))

(deftest advance-player
  (is (= (core/advance-player (core.Game. 0
                                          [(core.Player. "A" 0 [])
                                           (core.Player. "B" 0 [])
                                           (core.Player. "C" 0 [])]
                                          {}
                                          []))
         (core.Game. 0
                     [(core.Player. "B" 0 [])
                      (core.Player. "C" 0 [])
                      (core.Player. "A" 0 [])]
                     {}
                     []))))

(deftest advance-turn
  (is (= (core/advance-turn (core.Game. 0
                                        [(core.Player. "A" 0 [])
                                         (core.Player. "B" 0 [])
                                         (core.Player. "C" 0 [])]
                                        {}
                                        [2 3 4]))
         (core.Game. 0
                     [(core.Player. "B" 0 [])
                      (core.Player. "C" 0 [])
                      (core.Player. "A" 0 [])]
                     {}
                     [3 4]))))

(deftest new-scratch
  (is (= (core/new-scratch (core.Game. 0
                                       [(core.Player. "A" 10 [2 2])
                                        (core.Player. "B" 20 [2 3])
                                        (core.Player. "C" 30 [2 4])]
                                       {2 (core.Horse. 0 false)
                                        3 (core.Horse. 1 true)}
                                       [2 3 4]))
         (core.Game. 30
                     [(core.Player. "B" 10 [3])
                      (core.Player. "C" 20 [4])
                      (core.Player. "A" 0 [])]
                     {2 (core.Horse. 2 true)
                      3 (core.Horse. 1 true)}
                     [3 4]))))

(deftest pay-scratch
  (is (= (core/pay-scratch (core.Game. 0
                                       [(core.Player. "A" 5 [])
                                        (core.Player. "B" 0 [])
                                        (core.Player. "C" 0 [])]
                                       {2 (core.Horse. 2 true)
                                        3 (core.Horse. 1 true)}
                                       [2 3 4]))
         (core.Game. 5
                     [(core.Player. "B" 0 [])
                      (core.Player. "C" 0 [])
                      (core.Player. "A" 0 [])]
                     {2 (core.Horse. 2 true)
                      3 (core.Horse. 1 true)}
                     [3 4]))))

(deftest advance-horse
  (is (= (core/advance-horse (core.Game. 0
                                         [(core.Player. "A" 0 [])
                                          (core.Player. "B" 0 [])
                                          (core.Player. "C" 0 [])]
                                         {2 (core.Horse. 0 false)}
                                         [2 3 4]))
         (core.Game. 0
                     [(core.Player. "B" 0 [])
                      (core.Player. "C" 0 [])
                      (core.Player. "A" 0 [])]
                     {2 (core.Horse. 1 false)}
                     [3 4]))))

(deftest play-turn
  (is (= (core/play-turn (core.Game. 0
                                     [(core.Player. "A" 0 [])
                                      (core.Player. "B" 0 [])
                                      (core.Player. "C" 0 [])]
                                     {}
                                     [2 3 4]))
         (core.Game. 0
                     [(core.Player. "B" 0 [])
                      (core.Player. "C" 0 [])
                      (core.Player. "A" 0 [])]
                     {}
                     [2 3 4])))
  (is (= (core/play-turn (core.Game. 0
                                     [(core.Player. "A" 10 [])
                                      (core.Player. "B" 0 [])
                                      (core.Player. "C" 0 [])]
                                     {2 (core.Horse. 1 true)}
                                     [2 3 4]))
         (core.Game. 5
                     [(core.Player. "B" 0 [])
                      (core.Player. "C" 0 [])
                      (core.Player. "A" 5 [])]
                     {2 (core.Horse. 1 true)}
                     [3 4])))
  (is (= (core/play-turn (core.Game. 0
                                     [(core.Player. "A" 10 [])
                                      (core.Player. "B" 0 [])
                                      (core.Player. "C" 0 [])]
                                     {2 (core.Horse. 0 false)
                                      3 (core.Horse. 1 true)
                                      4 (core.Horse. 2 true)
                                      5 (core.Horse. 3 true)
                                      6 (core.Horse. 4 true)}
                                     [2 3 4]))
         (core.Game. 0
                     [(core.Player. "B" 0 [])
                      (core.Player. "C" 0 [])
                      (core.Player. "A" 10 [])]
                     {2 (core.Horse. 1 false)
                      3 (core.Horse. 1 true)
                      4 (core.Horse. 2 true)
                      5 (core.Horse. 3 true)
                      6 (core.Horse. 4 true)}
                     [3 4])))
  (is (= (core/play-turn (core.Game. 0
                                     [(core.Player. "A" 10 [2 2])
                                      (core.Player. "B" 20 [2 3])
                                      (core.Player. "C" 30 [2 4])]
                                     {2 (core.Horse. 0 false)}
                                     [2 3 4]))
         (core.Game. 20
                     [(core.Player. "B" 15 [3])
                      (core.Player. "C" 25 [4])
                      (core.Player. "A" 0 [])]
                     {2 (core.Horse. 1 true)}
                     [3 4]))))

(run-tests)
