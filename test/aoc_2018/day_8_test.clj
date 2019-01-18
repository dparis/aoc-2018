(ns aoc-2018.day-8-test
  (:require [aoc-2018.day-8 :as sut
             :refer [parse-data]]
            [clojure.test :refer :all]))


(def day-8-1-data
  [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(deftest day-8-1
  (testing "sum metadata"
    (is (= 138 (get (parse-data day-8-1-data) :metadata-sum))))

  (testing "root node value"
    (is (= 66 (-> (parse-data day-8-1-data)
                  (get-in [:child-sums :root])
                  (first))))))
