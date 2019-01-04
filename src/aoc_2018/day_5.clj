(ns aoc-2018.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def input
  (slurp (io/resource "day_5_1_input.txt")))

(defn ^:private parse-input
  [input]
  (str/trim input))

(defn units-reactive?
  [units]
  (let [[a b] units]
    (if (Character/isLowerCase a)
      (= b (Character/toUpperCase a))
      (= b (Character/toLowerCase a)))))

(defn reduce-polymer
  [polymer]
  (loop [polymer-head []
         polymer-tail (seq polymer)]
    (if (< (count (take 2 polymer-tail)) 2)
      (concat polymer-head polymer-tail)
      (let [units        (take 2 polymer-tail)
            polymer-rest (nthrest polymer-tail 2)]
        (if (units-reactive? units)
          (recur (vec (butlast polymer-head))
                 (if-let [previous-unit (last polymer-head)]
                   (cons previous-unit polymer-rest)
                   polymer-rest))
          (recur (conj polymer-head (first units))
                 (rest polymer-tail)))))))

(defn ^:private calculate-reduced-polymer
  ([polymer] (calculate-reduced-polymer polymer 2000))
  ([polymer partition-size]
   (->> (partition-all partition-size polymer)
        (pmap reduce-polymer)
        (apply concat)
        (reduce-polymer))))

(defn calculate-reduced-polymer-unit-count
  [input]
  (let [polymer (parse-input input)]
    (count (calculate-reduced-polymer polymer))))

(defn ^:private filter-units
  [polymer unit]
  (let [pattern (re-pattern (format "[%s%s]" unit (Character/toUpperCase unit)))]
    (str/replace polymer pattern "")))

(defn remove-trouble-polymer
  [polymer]
  (let [unit-set (set (map #(Character/toLowerCase %) polymer))]
    (->> (for [unit unit-set
               :let [filtered-poly (filter-units polymer unit)
                     reduced-poly  (calculate-reduced-polymer filtered-poly)]]
           [unit (count reduced-poly) reduced-poly])
         (sort-by second)
         (first))))

(defn calculate-filtered-reduced-polymer-unit-count
  [input]
  (let [polymer                         (parse-input input)
        [problem-unit unit-count final] (remove-trouble-polymer polymer)]
    unit-count))
