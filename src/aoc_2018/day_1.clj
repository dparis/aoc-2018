(ns aoc-2018.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def day-1-input
  (slurp (io/resource "day_1_1_input.txt")))

(defn ^:private parse-input
  [input]
  (let [changes (-> input
                    (str/replace #"\+" "")
                    (str/split #"\n"))]
    (map #(Integer/parseInt %) changes)))

(defn frequency
  [changes]
  (apply + changes))

(defn calculate-frequency-1
  [input]
  (let [changes (parse-input input)]
    (frequency changes)))

(defn first-repeated-frequency
  [changes]
  (loop [remaining-changes changes
         current-freq      0
         seen-freqs        #{0}]
    (let [new-freq (+ current-freq (first remaining-changes))]
      (if (contains? seen-freqs new-freq)
        new-freq
        (recur (or (next remaining-changes) changes)
               new-freq
               (conj seen-freqs new-freq))))))

(defn calculate-frequency-2
  [input]
  (let [changes (parse-input input)]
    (first-repeated-frequency changes)))
