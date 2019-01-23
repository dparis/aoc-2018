(ns aoc-2018.day-10-test
  (:require [aoc-2018.day-10 :as sut
             :refer [search-for-message]]
            [clojure.test :refer :all]))


(def ^:private day-10-1-beacons
  [{:id 0  :position-x 9  :position-y 1  :velocity-x 0  :velocity-y 2}
   {:id 1  :position-x 7  :position-y 0  :velocity-x -1 :velocity-y 0}
   {:id 2  :position-x 3  :position-y -2 :velocity-x -1 :velocity-y 1}
   {:id 3  :position-x 6  :position-y 10 :velocity-x -2 :velocity-y -1}
   {:id 4  :position-x 2  :position-y -4 :velocity-x 2  :velocity-y 2}
   {:id 5  :position-x -6 :position-y 10 :velocity-x 2  :velocity-y -2}
   {:id 6  :position-x 1  :position-y 8  :velocity-x 1  :velocity-y -1}
   {:id 7  :position-x 1  :position-y 7  :velocity-x 1  :velocity-y 0}
   {:id 8  :position-x -3 :position-y 11 :velocity-x 1  :velocity-y -2}
   {:id 9  :position-x 7  :position-y 6  :velocity-x -1 :velocity-y -1}
   {:id 10 :position-x -2 :position-y 3  :velocity-x 1  :velocity-y 0}
   {:id 11 :position-x -4 :position-y 3  :velocity-x 2  :velocity-y 0}
   {:id 12 :position-x 10 :position-y -3 :velocity-x -1 :velocity-y 1}
   {:id 13 :position-x 5  :position-y 11 :velocity-x 1  :velocity-y -2}
   {:id 14 :position-x 4  :position-y 7  :velocity-x 0  :velocity-y -1}
   {:id 15 :position-x 8  :position-y -2 :velocity-x 0  :velocity-y 1}
   {:id 16 :position-x 15 :position-y 0  :velocity-x -2 :velocity-y 0}
   {:id 17 :position-x 1  :position-y 6  :velocity-x 1  :velocity-y 0}
   {:id 18 :position-x 8  :position-y 9  :velocity-x 0  :velocity-y -1}
   {:id 19 :position-x 3  :position-y 3  :velocity-x -1 :velocity-y 1}
   {:id 20 :position-x 0  :position-y 5  :velocity-x 0  :velocity-y -1}
   {:id 21 :position-x -2 :position-y 2  :velocity-x 2  :velocity-y 0}
   {:id 22 :position-x 5  :position-y -2 :velocity-x 1  :velocity-y 2}
   {:id 23 :position-x 1  :position-y 4  :velocity-x 2  :velocity-y 1}
   {:id 24 :position-x -2 :position-y 7  :velocity-x 2  :velocity-y -2}
   {:id 25 :position-x 3  :position-y 6  :velocity-x -1 :velocity-y -1}
   {:id 26 :position-x 5  :position-y 0  :velocity-x 1  :velocity-y 0}
   {:id 27 :position-x -6 :position-y 0  :velocity-x 2  :velocity-y 0}
   {:id 28 :position-x 5  :position-y 9  :velocity-x 1  :velocity-y -2}
   {:id 29 :position-x 14 :position-y 7  :velocity-x -2 :velocity-y 0}
   {:id 30 :position-x -3 :position-y 6  :velocity-x 2  :velocity-y -1}])

(def day-10-1-message-beacons
  [{:id 0  :position-x 9 :position-y 7 :velocity-x 0  :velocity-y 2}
   {:id 1  :position-x 4 :position-y 0 :velocity-x -1 :velocity-y 0}
   {:id 2  :position-x 0 :position-y 1 :velocity-x -1 :velocity-y 1}
   {:id 3  :position-x 0 :position-y 7 :velocity-x -2 :velocity-y -1}
   {:id 4  :position-x 8 :position-y 2 :velocity-x 2  :velocity-y 2}
   {:id 5  :position-x 0 :position-y 4 :velocity-x 2  :velocity-y -2}
   {:id 6  :position-x 4 :position-y 5 :velocity-x 1  :velocity-y -1}
   {:id 7  :position-x 4 :position-y 7 :velocity-x 1  :velocity-y 0}
   {:id 8  :position-x 0 :position-y 5 :velocity-x 1  :velocity-y -2}
   {:id 9  :position-x 4 :position-y 3 :velocity-x -1 :velocity-y -1}
   {:id 10 :position-x 1 :position-y 3 :velocity-x 1  :velocity-y 0}
   {:id 11 :position-x 2 :position-y 3 :velocity-x 2  :velocity-y 0}
   {:id 12 :position-x 7 :position-y 0 :velocity-x -1 :velocity-y 1}
   {:id 13 :position-x 8 :position-y 5 :velocity-x 1  :velocity-y -2}
   {:id 14 :position-x 4 :position-y 4 :velocity-x 0  :velocity-y -1}
   {:id 15 :position-x 8 :position-y 1 :velocity-x 0  :velocity-y 1}
   {:id 16 :position-x 9 :position-y 0 :velocity-x -2 :velocity-y 0}
   {:id 17 :position-x 4 :position-y 6 :velocity-x 1  :velocity-y 0}
   {:id 18 :position-x 8 :position-y 6 :velocity-x 0  :velocity-y -1}
   {:id 19 :position-x 0 :position-y 6 :velocity-x -1 :velocity-y 1}
   {:id 20 :position-x 0 :position-y 2 :velocity-x 0  :velocity-y -1}
   {:id 21 :position-x 4 :position-y 2 :velocity-x 2  :velocity-y 0}
   {:id 22 :position-x 8 :position-y 4 :velocity-x 1  :velocity-y 2}
   {:id 23 :position-x 7 :position-y 7 :velocity-x 2  :velocity-y 1}
   {:id 24 :position-x 4 :position-y 1 :velocity-x 2  :velocity-y -2}
   {:id 25 :position-x 0 :position-y 3 :velocity-x -1 :velocity-y -1}
   {:id 26 :position-x 8 :position-y 0 :velocity-x 1  :velocity-y 0}
   {:id 27 :position-x 0 :position-y 0 :velocity-x 2  :velocity-y 0}
   {:id 28 :position-x 8 :position-y 3 :velocity-x 1  :velocity-y -2}
   {:id 29 :position-x 8 :position-y 7 :velocity-x -2 :velocity-y 0}
   {:id 30 :position-x 3 :position-y 3 :velocity-x 2  :velocity-y -1}])

(deftest day-10-1
  (testing "search for message"
    (let [message-data (search-for-message day-10-1-beacons)]
      (is (= 3 (:seconds message-data)))
      (is (= day-10-1-message-beacons (sort-by :id (:beacons message-data)))))))
