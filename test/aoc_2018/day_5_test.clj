(ns aoc-2018.day-5-test
  (:require [aoc-2018.day-5 :as sut
             :refer [reduce-polymer remove-trouble-polymer]]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(def day-5-1-polymer-1
  "dabAcCaCBAcCcaDA")

(def day-5-1-polymer-2
  "ksKkDDdaAdrRzZlkKLWwiInmMneEPIipBbNjJFfVvNv")

(deftest day-4-1
  (testing "reduce polymer"
    (is (= "dabCBAcaDA" (str/join (reduce-polymer day-5-1-polymer-1))))
    (is (= "ksv" (str/join (reduce-polymer day-5-1-polymer-2)))))

  (testing "remove trouble polymer"
    (is (= [\c 4 '(\d \a \D \A)] (remove-trouble-polymer day-5-1-polymer-1)))
    (is (= [\k 2 '(\s \v)] (remove-trouble-polymer day-5-1-polymer-2)))))
