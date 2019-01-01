(ns aoc-2018.core
  (:require [aoc-2018.day-1 :as day-1]
            [aoc-2018.day-2 :as day-2]
            [clojure.pprint :as pp])
  (:gen-class))

(defn ^:private build-result
  [day first-answer second-answer]
  {"Day"                day
   "First Star Answer"  first-answer
   "Second Star Answer" second-answer})

(defn ^:private print-result-table
  []
  (let [results (vector
                 (build-result 1 (day-1/calculate-frequency-1 day-1/input)
                                 (day-1/calculate-frequency-2 day-1/input))
                 (build-result 2 (day-2/calculate-checksum day-2/input)
                                 (day-2/calculate-prototype-id day-2/input)))]
    (pp/print-table ["Day" "First Star Answer" "Second Star Answer"] results)))

(defn -main
  [& args]
  (print-result-table))
