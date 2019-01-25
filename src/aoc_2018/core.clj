(ns aoc-2018.core
  (:require [aoc-2018.day-1 :as day-1]
            [aoc-2018.day-2 :as day-2]
            [aoc-2018.day-3 :as day-3]
            [aoc-2018.day-4 :as day-4]
            [aoc-2018.day-5 :as day-5]
            [aoc-2018.day-6 :as day-6]
            [aoc-2018.day-7 :as day-7]
            [aoc-2018.day-8 :as day-8]
            [aoc-2018.day-9 :as day-9]
            [aoc-2018.day-10 :as day-10]
            [aoc-2018.day-11 :as day-11]
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

(defn ^:private day-7-result
  []
  ;; ~55 msecs
  (build-result
   7
   (day-7/calculate-step-order day-7/input)
   (day-7/calculate-parallel-step-order-seconds day-7/input)))

(defn ^:private day-8-result
  []
  ;; ~27 msecs
  (build-result
   8
   (day-8/calculate-license-metadata-sum day-8/input)
   (day-8/calculate-root-node-value day-8/input)))

(defn ^:private day-9-result
  []
  ;; ~9 seconds
  (build-result
   9
   (day-9/calculate-winning-score day-9/input)
   (day-9/calculate-winning-score-100x day-9/input)))

(defn ^:private day-10-result
  []
  ;; ~4.5 seconds
  (let [[message seconds] (day-10/calculate-message! day-10/input)]
    (build-result
     10
     message
     seconds)))

(defn ^:private day-11-result
  []
  ;; ~3.5 seconds
  (build-result
   11
   (day-11/calculate-highest-3x3-power-coordinate day-11/input)
   (day-11/calculate-highest-power-coordinate-and-size day-11/input)))

(defn ^:private print-result-table
  []
  (let [results (->> (vector (future (day-1-result))
                             (future (day-2-result))
                             (future (day-3-result))
                             (future (day-4-result))
                             (future (day-5-result))
                             (future (day-6-result))
                             (future (day-7-result))
                             (future (day-8-result))
                             (future (day-9-result))
                             (future (day-10-result))
                             (future (day-11-result)))
                     (mapv deref))]
    (pp/print-table ["Day" "First Star Answer" "Second Star Answer"] results)))

(defn -main
  [& args]
  (time
   (print-result-table)))
