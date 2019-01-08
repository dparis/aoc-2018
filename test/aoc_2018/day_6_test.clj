(ns aoc-2018.day-6-test
  (:require [aoc-2018.day-6 :as sut
             :refer [init-grid
                     parse-input
                     find-largest-coord-area
                     find-closest-region-area]]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(def day-6-1-coords
  "1, 1
   1, 6
   8, 3
   3, 4
   5, 5
   8, 9")

(deftest day-6-1
  (testing "find largest coordiate area"
    (let [grid (init-grid (parse-input day-6-1-coords))]
      (is (= 17 (find-largest-coord-area grid)))))

  (testing "find closest region area"
    (let [grid (init-grid (parse-input day-6-1-coords))]
      (is (= 16 (find-closest-region-area grid 32))))))
