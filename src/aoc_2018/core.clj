(ns aoc-2018.core
  (:require [aoc-2018.day-1 :as day-1]
            [aoc-2018.day-2 :as day-2]
            [aoc-2018.day-3 :as day-3]
            [aoc-2018.day-4 :as day-4]
            [aoc-2018.day-5 :as day-5]
            [clojure.pprint :as pp])
  (:gen-class))

(defn ^:private build-result
  [day first-answer second-answer]
  {"Day"                day
   "First Star Answer"  first-answer
   "Second Star Answer" second-answer})

(defn ^:private day-1-result
  []
  (build-result
   1
   (day-1/calculate-frequency-1 day-1/input)
   (day-1/calculate-frequency-2 day-1/input)))

(defn ^:private day-2-result
  []
  (build-result
   2
   (day-2/calculate-checksum day-2/input)
   (day-2/calculate-prototype-id day-2/input)))

(defn ^:private day-3-result
  []
  (build-result
   3
   (day-3/calculate-overlapping-square-inches day-3/input)
   (day-3/calculate-non-overlapping-claim-id day-3/input)))

(defn ^:private day-4-result
  []
  (build-result
   4
   (day-4/calculate-guard-minute day-4/input)
   (day-4/calculate-minute-guard day-4/input)))

(defn ^:private day-5-result
  []
  (build-result
   5
   (day-5/calculate-reduced-polymer-unit-count day-5/input)
   (day-5/calculate-filtered-reduced-polymer-unit-count day-5/input)))

(defn ^:private print-result-table
  []
  (let [results (vector (day-1-result)
                        (day-2-result)
                        (day-3-result)
                        (day-4-result)
                        (day-5-result))]
    (pp/print-table ["Day" "First Star Answer" "Second Star Answer"] results)))

(defn -main
  [& args]
  (print-result-table))
