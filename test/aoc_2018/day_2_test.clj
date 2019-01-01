(ns aoc-2018.day-2-test
  (:require [aoc-2018.day-2 :as sut
             :refer [checksum
                     find-mismatched-id-pair
                     find-prototype-id
                     calculate-prototype-id]]
            [clojure.test :refer :all]))


(def day-2-1-ids
  ["abcdef"
   "bababc"
   "abbcde"
   "abcccd"
   "aabcdd"
   "abcdee"
   "ababab"])

(def day-2-2-ids
  ["abcde"
   "fghij"
   "klmno"
   "pqrst"
   "fguij"
   "axcye"
   "wvxyz"])

(deftest day-2-1
  (testing "generate checksum"
    (is (= 12 (checksum day-2-1-ids))))

  (testing "find mismatched id pair"
    (is (= ["fghij" "fguij"] (find-mismatched-id-pair day-2-2-ids))))

  (testing "find prototype id"
    (is (= "fgij" (find-prototype-id day-2-2-ids)))))
