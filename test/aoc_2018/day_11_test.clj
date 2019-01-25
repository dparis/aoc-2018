(ns aoc-2018.day-11-test
  (:require [aoc-2018.day-11 :as sut
             :refer [highest-power
                     initialize-power-grid
                     initialize-sat]]
            [clojure.test :refer :all]))


(deftest day-11-1
  (testing "find highest power square"
    (let [sat-18 (-> (initialize-power-grid 18) (initialize-sat))
          sat-42 (-> (initialize-power-grid 42) (initialize-sat))]
      (is (= [[33 45] 29] (highest-power sat-18 3)))
      (is (= [[21 61] 30] (highest-power sat-42 3))))))
