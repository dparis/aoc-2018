(ns aoc-2018.day-1-test
  (:require [aoc-2018.day-1 :as sut
             :refer [frequency first-repeated-frequency]]
            [clojure.test :refer :all]))


(deftest day-1-1
  (testing "calculate frequency"
    (is (= (frequency [1 -2 3 1]) 3))
    (is (= (frequency [1 1 1]) 3))
    (is (= (frequency [1 1 -2]) 0))
    (is (= (frequency [-1 -2 -3]) -6))))

(deftest day-1-2
  (testing "calculate first repeating frequency"
    (is (= (first-repeated-frequency [1 -2 3 1]) 2))
    (is (= (first-repeated-frequency [1 -1]) 0))
    (is (= (first-repeated-frequency [3, 3, 4, -2, -4]) 10))
    (is (= (first-repeated-frequency [-6, 3, 8, 5, -6]) 5))
    (is (= (first-repeated-frequency [7, 7, -2, -7, -4]) 14))))
