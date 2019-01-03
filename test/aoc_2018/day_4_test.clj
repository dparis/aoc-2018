(ns aoc-2018.day-4-test
  (:require [aoc-2018.day-4 :as sut
             :refer [parse-input
                     compile-guard-data
                     find-guard-minute
                     find-minute-guard]]
            [clojure.test :refer :all]))

(def day-4-1-log-entries
  "[1518-11-01 00:00] Guard #10 begins shift
   [1518-11-01 00:05] falls asleep
   [1518-11-01 00:25] wakes up
   [1518-11-01 00:30] falls asleep
   [1518-11-01 00:55] wakes up
   [1518-11-01 23:58] Guard #99 begins shift
   [1518-11-02 00:40] falls asleep
   [1518-11-02 00:50] wakes up
   [1518-11-03 00:05] Guard #10 begins shift
   [1518-11-03 00:24] falls asleep
   [1518-11-03 00:29] wakes up
   [1518-11-04 00:02] Guard #99 begins shift
   [1518-11-04 00:36] falls asleep
   [1518-11-04 00:46] wakes up
   [1518-11-05 00:03] Guard #99 begins shift
   [1518-11-05 00:45] falls asleep
   [1518-11-05 00:55] wakes up")

(def log-events
  (parse-input day-4-1-log-entries))

(deftest day-4-1
  (testing "compile guard data"
    (let [guard-data (compile-guard-data log-events)]
      (is (= #{10 99} (set (keys guard-data))))

      (testing "for guard 10"
        (let [data (get guard-data 10)]
          (is (= 2 (count (:shifts data))))
          (is (= 50 (count (:sleep-minutes data))))))

      (testing "for guard 99"
        (let [data (get guard-data 99)]
          (is (= 3 (count (:shifts data))))
          (is (= 30 (count (:sleep-minutes data))))))))

  (testing "find sleepiest guard-minute"
    (is (= 240 (find-guard-minute log-events))))

  (testing "find most frequently slept minute-guard"
    (is (= 4455 (find-minute-guard log-events)))))
