(ns aoc-2018.day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def input
  (slurp (io/resource "day_8_1_input.txt")))

(defn parse-input
  [input]
  (->> (str/split input #"\s")
       (mapv #(Integer/parseInt %))))

(def ^:private node-ids
  (let [letters ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
                 "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"]]
    (vec
     (for [x letters y letters z letters]
       (str x y z)))))

(def ^:private initial-state
  {:cursor       0
   :child-sums   {}
   :metadata-sum 0
   :node-count   0})

(defn ^:private sum-child-values
  [state node-id metadata]
  (let [child-sums (get-in state [:child-sums node-id])]
    (->> (map #(nth child-sums (dec %) nil) metadata)
         (remove nil?)
         (apply +))))

(defn parse-data
  ([data]
   (parse-data data :root initial-state))
  ([data parent-id state]
   (let [cur-tree                    (:tree state)
         cur-cursor                  (:cursor state)
         post-node-cursor            (+ cur-cursor 2)
         [num-children num-metadata] (subvec data cur-cursor post-node-cursor)
         node-id                     (nth node-ids (:node-count state))
         updated-state               (-> state
                                         (assoc :cursor post-node-cursor)
                                         (update :node-count inc))
         post-children-state         (->> updated-state
                                          (iterate (partial parse-data data node-id))
                                          (take (inc num-children))
                                          (last))
         metadata                    (->> (:cursor post-children-state)
                                          (subvec data)
                                          (take num-metadata)
                                          (vec))
         node-value                  (if (= 0 num-children)
                                       (reduce + metadata)
                                       (sum-child-values post-children-state
                                                         node-id
                                                         metadata))]
     (-> post-children-state
         (update :cursor + num-metadata)
         (update :metadata-sum + (apply + metadata))
         (update-in [:child-sums parent-id] (fnil conj []) node-value)))))

(defn calculate-license-metadata-sum
  [input]
  (let [data (parse-input input)]
    (-> (parse-data data)
        (get :metadata-sum))))

(defn calculate-root-node-value
  [input]
  (let [data (parse-input input)]
    (-> (parse-data data)
        (get-in [:child-sums :root])
        (first))))
