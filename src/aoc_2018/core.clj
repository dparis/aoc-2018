(ns aoc-2018.core
  (:require [aoc-2018.day-1 :as day-1]
            [aoc-2018.day-2 :as day-2]
            [aoc-2018.day-3 :as day-3]
            [aoc-2018.day-4 :as day-4]
            [aoc-2018.day-5 :as day-5]
            [aoc-2018.day-6 :as day-6]
            [clojure.pprint :as pp])
  (:gen-class))

(defn ^:private build-result
  [day first-answer second-answer]
  {"Day"                day
   "First Star Answer"  first-answer
   "Second Star Answer" second-answer})

(defn ^:private day-1-result
  []
  ;; ~160 msecs
  (build-result
   1
   (day-1/calculate-frequency-1 day-1/input)
   (day-1/calculate-frequency-2 day-1/input)))

(defn ^:private day-2-result
  []
  ;; ~152 msecs
  (build-result
   2
   (day-2/calculate-checksum day-2/input)
   (day-2/calculate-prototype-id day-2/input)))

(defn ^:private day-3-result
  []
  ;; ~1.5 seconds
  (build-result
   3
   (day-3/calculate-overlapping-square-inches day-3/input)
   (day-3/calculate-non-overlapping-claim-id day-3/input)))

(defn ^:private day-4-result
  []
  ;; ~82 msecs
  (build-result
   4
   (day-4/calculate-guard-minute day-4/input)
   (day-4/calculate-minute-guard day-4/input)))

(defn ^:private day-5-result
  []
  ;; ~18 seconds
  (build-result
   5
   (day-5/calculate-reduced-polymer-unit-count day-5/input)
   (day-5/calculate-filtered-reduced-polymer-unit-count day-5/input)))

(defn ^:private day-6-result
  []
  ;; ~6 seconds
  (build-result
   6
   (day-6/calculate-largest-coord-area day-6/input)
   (day-6/calculate-closest-region-area day-6/input)))

(defn ^:private print-result-table
  []
  (let [results (->> (vector (future (day-1-result))
                             (future (day-2-result))
                             (future (day-3-result))
                             (future (day-4-result))
                             (future (day-5-result))
                             (future (day-6-result)))
                     (mapv deref))]
    (pp/print-table ["Day" "First Star Answer" "Second Star Answer"] results)))

(defn -main
  [& args]
  (time
   (print-result-table)))
