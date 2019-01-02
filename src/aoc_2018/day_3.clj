(ns aoc-2018.day-3
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as cmb]
            [clojure.set :as cs]
            [clojure.string :as str]
            [taoensso.tufte :as tuf :refer [profile p]]))


(def input
  (slurp (io/resource "day_3_1_input.txt")))

(defn ^:private parse-input
  [input]
  (str/split input #"\n"))

(def ^:private claim-regexp
  #"(\d+)\s*@\s*(\d+),(\d+)\s*:\s*(\d+)x(\d+)")

(defn parse-claim
  [claim]
  (let [[_ id x-offset y-offset x y] (re-find claim-regexp claim)]
    {:id       (Integer/parseInt id)
     :x-offset (Integer/parseInt x-offset)
     :y-offset (Integer/parseInt y-offset)
     :width    (Integer/parseInt x)
     :height   (Integer/parseInt y)}))

(defn ^:private claim-coordinates
  [claim]
  (let [x-offset (:x-offset claim)
        y-offset (:y-offset claim)
        width    (:width claim)
        height   (:height claim)]
    (for [x (range x-offset (+ x-offset width))
          y (range y-offset (+ y-offset height))]
      [x y])))

(defn coordinate-claim-lookup
  [claims]
  (reduce
   (fn [lookup claim]
     (let [id          (:id claim)
           coordinates (claim-coordinates claim)]
       (reduce
        (fn [inner-lookup coordinate]
          (update inner-lookup coordinate (fnil conj #{}) id))
        lookup
        coordinates)))
   {}
   claims))

(defn overlapping-coordinates
  [claims]
  (let [lookup (coordinate-claim-lookup claims)]
    (->> (filter (fn [[coordinate ids]] (> (count ids) 1)) lookup)
         (into {}))))

(defn calculate-overlapping-square-inches
  [input]
  (let [claims (map parse-claim (parse-input input))]
    (count (overlapping-coordinates claims))))

(defn claim-coordinate-lookup
  [claims]
  (reduce
   (fn [lookup claim]
     (assoc lookup (:id claim) (claim-coordinates claim)))
   {}
   claims))

(defn non-overlapping-claims-slow
  [claims]
  (let [lookup (claim-coordinate-lookup claims)]
    (loop [remaining-ids (keys lookup)
           claim-ids     #{}]
      (if-let [id (first remaining-ids)]
        (let [claim-coords (set (get lookup id))
              other-coords (->> (dissoc lookup id)
                                (vals)
                                (apply concat)
                                (set))]
          (if (empty? (cs/intersection claim-coords other-coords))
            (recur (next remaining-ids) (conj claim-ids id))
            (recur (next remaining-ids) claim-ids)))
        (select-keys lookup claim-ids)))))

(defn ^:private claims-overlap?
  [claim-a claim-b]
  (let [a-tl-x (:x-offset claim-a)
        a-tl-y (:y-offset claim-a)
        a-br-x (+ (:x-offset claim-a) (dec (:width claim-a)))
        a-br-y (+ (:y-offset claim-a) (dec (:height claim-a)))
        b-tl-x (:x-offset claim-b)
        b-tl-y (:y-offset claim-b)
        b-br-x (+ (:x-offset claim-b) (dec (:width claim-b)))
        b-br-y (+ (:y-offset claim-b) (dec (:height claim-b)))]
    (cond
      (or (> a-tl-x b-br-x) (> b-tl-x a-br-x)) false
      (or (> a-tl-y b-br-y) (> b-tl-y a-br-y)) false
      :else                                    true)))

(defn non-overlapping-claims
  ([claims] (non-overlapping-claims claims true))
  ([claims only-first?]
   (let [claim-set (set claims)]
     (reduce
      (fn [non-overlapping-set claim]
        (let [other-set (disj claim-set claim)]
          (if (some #(claims-overlap? claim %) other-set)
            non-overlapping-set
            (if only-first?
              (reduced (conj non-overlapping-set claim))
              (conj non-overlapping-set claim)))))
      #{}
      claims))))

(defn calculate-non-overlapping-claim-id
  [input]
  (let [claims (map parse-claim (parse-input input))]
    (-> (non-overlapping-claims claims)
        (first)
        (get :id))))
