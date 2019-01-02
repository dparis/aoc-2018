(ns aoc-2018.day-3-test
  (:require [aoc-2018.day-3 :as sut
             :refer [parse-claim
                     overlapping-coordinates
                     non-overlapping-claims]]
            [clojure.test :refer :all]))


(def day-3-1-claims
  ["#1 @ 1,3: 4x4"
   "#2 @ 3,1: 4x4"
   "#3 @ 5,5: 2x2"])

(deftest day-3-1
  (testing "find number of overlapping square inches"
    (let [claims               (map parse-claim day-3-1-claims)
          expected-coordinates {[4 3] #{1 2}
                                [3 3] #{1 2}
                                [3 4] #{1 2}
                                [4 4] #{1 2}}]
      (is (= expected-coordinates (overlapping-coordinates claims)))))

  (testing "find non-overlapping coordinates"
    (let [claims          (map parse-claim day-3-1-claims)
          expected-claims #{{:id 3, :x-offset 5, :y-offset 5, :width 2, :height 2}}]
      (is (= expected-claims (non-overlapping-claims claims))))))
