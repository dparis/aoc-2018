(ns aoc-2018.day-2
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as cmb]
            [clojure.string :as str]
            [taoensso.tufte :as tuf :refer [profile p]]))


(def day-2-input
  (slurp (io/resource "day_2_1_input.txt")))

(defn parse-input
  [input]
  (str/split input #"\n"))

(defn ^:private id-2-freq?
  [id-char-freqs]
  (contains? (set (vals id-char-freqs)) 2))

(defn ^:private id-3-freq?
  [id-char-freqs]
  (contains? (set (vals id-char-freqs)) 3))

(defn checksum
  [ids]
  (loop [remaining-ids ids
         len-2-ids     []
         len-3-ids     []]
    (if-let [id (first remaining-ids)]
      (let [id-char-freqs (frequencies (seq id))]
        (recur (rest remaining-ids)
               (if (id-2-freq? id-char-freqs)
                 (conj len-2-ids id)
                 len-2-ids)
               (if (id-3-freq? id-char-freqs)
                 (conj len-3-ids id)
                 len-3-ids)))
      (* (count len-2-ids)
         (count len-3-ids)))))

(defn calculate-checksum
  [input]
  (let [ids (parse-input input)]
    (checksum ids)))

(defn ^:private differ-by-one?
  [id-1 id-2]
  (and (= (count id-1) (count id-2))
       (= 1 (->> (map vector id-1 id-2)
                 (filter (fn [[x y]] (not= x y)))
                 (count)))))

(defn find-mismatched-id-pair
  [ids]
  (->> (cmb/cartesian-product ids ids)
       (filter (fn [[id-1 id-2]] (differ-by-one? id-1 id-2)))
       (first)
       (vec)))

(defn find-prototype-id
  [ids]
  (let [[id-1 id-2] (find-mismatched-id-pair ids)]
    (when (and id-1 id-2)
      (->> (mapv #(when (= %1 %2) %1) id-1 id-2)
           (remove nil?)
           (apply str)))))

(defn calculate-prototype-id
  [input]
  (let [ids (parse-input input)]
    (find-prototype-id ids)))
