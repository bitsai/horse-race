(ns tests
  (:require [core :as core])
  (:use [clojure.test]))

;; Player functions
(deftest dead?
  (is (core/dead? (core.Player. "A" 0 [])))
  (is (not (core/dead? (core.Player. "A" 10 [])))))

(deftest deal-cards
  (is (= (core/deal-cards [(core.Player. "A" 0 [])
                           (core.Player. "B" 0 [])
                           (core.Player. "C" 0 [])]
                          core/ranks)
         [(core.Player. "A" 0 [2 5 8 11])
          (core.Player. "B" 0 [3 6 9 12])
          (core.Player. "C" 0 [4 7 10])])))

(deftest count-cards
  (is (= (core/count-cards (core.Player. "A" 0 [2 2 3 3 4 4]) 2)
         2)))

(deftest discard-card
  (is (= (core/discard-card [(core.Player. "A" 0 [0 0 0])
                             (core.Player. "B" 0 [0 0 1])
                             (core.Player. "C" 0 [0 1 1])]
                            1)
         [(core.Player. "A" 0 [0 0 0])
          (core.Player. "B" 0 [0 0])
          (core.Player. "C" 0 [0])])))

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

(deftest pay-share
  (is (= (core/pay-share (core.Player. "A" 0 []) 10)
         [(core.Player. "A" 0 []) 0]))
  (is (= (core/pay-share (core.Player. "A" 10 []) 10)
         [(core.Player. "A" 20 []) 10])))

(deftest pay-pot
  (is (= (core/pay-pot [(core.Player. "A" 20 [2 2])
                        (core.Player. "B" 10 [2 3])
                        (core.Player. "C"  0 [2 4])]
                       2
                       100)
         [[(core.Player. "A" 70 [2 2])
           (core.Player. "B" 35 [2 3])
           (core.Player. "C"  0 [2 4])]
          25])))

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

(deftest winner
  (is (= (core/winner {2 (core.Horse. 3 true)
                       3 (core.Horse. 4 true)
                       4 (core.Horse. 5 false)})
         [4 (core.Horse. 5 false)])))

;; Game functions
(deftest new-race
  (is (= (core/new-race (core.Game. 0
                                    [(core.Player. "A" 20 [])
                                     (core.Player. "B" 10 [])
                                     (core.Player. "C"  0 [])]
                                    {}
                                    [])
                        core/ranks)
         (core.Game. 0
                     [(core.Player. "A" 20 [2 5 8 11])
                      (core.Player. "B" 10 [3 6 9 12])
                      (core.Player. "C"  0 [4 7 10])]
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
                     []))))

(deftest new-game
  (is (= (core/new-game ["A" "B" "C"] 20 core/ranks [2 3 4])
         (core.Game. 0
                     [(core.Player. "A" 20 [2 5 8 11])
                      (core.Player. "B" 20 [3 6 9 12])
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

(deftest race-over?
  (is (core/race-over? (core.Game. 0
                                   [(core.Player. "A" 0 [])]
                                   {2 (core.Horse. 0 false)}
                                   [])))
  (is (not (core/race-over? (core.Game. 0
                                        [(core.Player. "A" 5 [])]
                                        {2 (core.Horse. 0 false)}
                                        []))))
  (is (core/race-over? (core.Game. 0
                                   [(core.Player. "A" 10 [])]
                                   {2 (core.Horse. 3 false)}
                                   [])))
  (is (not (core/race-over? (core.Game. 0
                                        [(core.Player. "A" 10 [])]
                                        {2 (core.Horse. 3 true)}
                                        [])))))

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
                     [(core.Player. "A" 0 [])
                      (core.Player. "B" 10 [3])
                      (core.Player. "C" 20 [4])]
                     {2 (core.Horse. 2 true)
                      3 (core.Horse. 1 true)}
                     [2 3 4]))))

(deftest pay-scratch
  (is (= (core/pay-scratch (core.Game. 0
                                       [(core.Player. "A" 5 [])]
                                       {2 (core.Horse. 2 true)
                                        3 (core.Horse. 1 true)}
                                       [2 3 4]))
         (core.Game. 5
                     [(core.Player. "A" 0 [])]
                     {2 (core.Horse. 2 true)
                      3 (core.Horse. 1 true)}
                     [2 3 4]))))

(deftest move-horse
  (is (= (core/move-horse (core.Game. 0
                                      []
                                      {2 (core.Horse. 0 false)}
                                      [2 3 4]))
         (core.Game. 0
                     []
                     {2 (core.Horse. 1 false)}
                     [2 3 4]))))

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

(deftest split-pot
  (is (= (core/split-pot (core.Game. 100
                                     [(core.Player. "B" 10 [2 3])
                                      (core.Player. "C"  0 [2 4])
                                      (core.Player. "A" 20 [2 2])]
                                     {2 (core.Horse. 3 false)}
                                     [3 4]))
         (core.Game. 25
                     [(core.Player. "B" 35 [2 3])
                      (core.Player. "C"  0 [2 4])
                      (core.Player. "A" 70 [2 2])]
                     {2 (core.Horse. 3 false)}
                     [3 4]))))

(deftest play-race
  (is (= (core/play-race (core.Game. 100
                                     [(core.Player. "A" 20 [2 2])
                                      (core.Player. "B" 10 [2 3])
                                      (core.Player. "C"  0 [2 4])]
                                     {2 (core.Horse. 2 false)
                                      3 (core.Horse. 1 true)
                                      4 (core.Horse. 2 true)
                                      5 (core.Horse. 3 true)
                                      6 (core.Horse. 4 true)}
                                     [2 3 4]))
         [(core.Game. 100
                      [(core.Player. "A" 20 [2 2])
                       (core.Player. "B" 10 [2 3])
                       (core.Player. "C"  0 [2 4])]
                      {2 (core.Horse. 2 false)
                       3 (core.Horse. 1 true)
                       4 (core.Horse. 2 true)
                       5 (core.Horse. 3 true)
                       6 (core.Horse. 4 true)}
                      [2 3 4])
          (core.Game. 100
                      [(core.Player. "B" 10 [2 3])
                       (core.Player. "C"  0 [2 4])
                       (core.Player. "A" 20 [2 2])]
                      {2 (core.Horse. 3 false)
                       3 (core.Horse. 1 true)
                       4 (core.Horse. 2 true)
                       5 (core.Horse. 3 true)
                       6 (core.Horse. 4 true)}
                      [3 4])
          (core.Game. 25
                      [(core.Player. "B" 35 [2 3])
                       (core.Player. "C"  0 [2 4])
                       (core.Player. "A" 70 [2 2])]
                      {2 (core.Horse. 3 false)
                       3 (core.Horse. 1 true)
                       4 (core.Horse. 2 true)
                       5 (core.Horse. 3 true)
                       6 (core.Horse. 4 true)}
                      [3 4])])))

(run-tests)
