(ns aoc-2018.day-6
  (:require [com.climate.claypoole :as cp]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as cmb]
            [clojure.string :as str]
            [taoensso.tufte :as tuf
             :refer [profile p]]))


(def input
  (slurp (io/resource "day_6_1_input.txt")))

(defn ^:private parse-line
  [line]
  (->> (str/split line #", ")
       (mapv #(Integer/parseInt %))))

(defn ^:private parse-input
  [input]
  (->> (str/split input #"\n")
       (mapv parse-line)
       (sort)))

(defn ^:private min-x-coord
  [coords]
  (first (sort-by first coords)))

(defn ^:private min-y-coord
  [coords]
  (first (sort-by second coords)))

(defn ^:private max-x-coord
  [coords]
  (last (sort-by first coords)))

(defn ^:private max-y-coord
  [coords]
  (last (sort-by second coords)))

(defn grid-coords
  [top-left-coord bottom-right-coord]
  (let [min-x-val (first top-left-coord)
        min-y-val (second top-left-coord)
        max-x-val (first bottom-right-coord)
        max-y-val (second bottom-right-coord)
        xs        (range min-x-val (inc max-x-val))
        ys        (range min-y-val (inc max-y-val))]
    (mapv vec (cmb/cartesian-product xs ys))))

(defn init-grid
  [coords]
  (let [coords-set       (set coords)
        min-x            (min-x-coord coords)
        min-y            (min-y-coord coords)
        max-x            (max-x-coord coords)
        max-y            (max-y-coord coords)
        min-coords       (filter #(or (= (first min-x) (first %))
                                      (= (second min-y) (second %)))
                                 coords)
        max-coords       (filter #(or (= (first max-x) (first %))
                                      (= (second max-y) (second %)))
                                 coords)
        candidate-coords (apply disj coords-set (concat min-coords max-coords))
        tl-coord         (vector (first min-x) (second min-y))
        br-coord         (vector (first max-x) (second max-y))]
    {:all-coords         coords-set
     :candidate-coords   candidate-coords
     :top-left-coord     tl-coord
     :bottom-right-coord br-coord
     :grid-coords        (grid-coords tl-coord br-coord)}))

(defn ^:private man-dist
  [a b]
  (let [^int x1 (nth a 0)
        ^int y1 (nth a 1)
        ^int x2 (nth b 0)
        ^int y2 (nth b 1)
        x-dist  (- x1 x2)
        y-dist  (- y1 y2)]
    (+ (Math/abs x-dist)
       (Math/abs y-dist))))

(defn ^:private closest-coord
  [comparison-coords basis-coord]
  (let [distances (->> (for [comp-coord comparison-coords]
                         [comp-coord (man-dist comp-coord basis-coord)])
                       (sort-by second))
        [d1 d2]   (take 2 distances)]
    (when-not (= (second d1) (second d2))
      (first d1))))

(defn ^:private area-bounded?
  [grid owned-coords]
  (let [[x-min y-min] (:top-left-coord grid)
        [x-max y-max] (:bottom-right-coord grid)]
    (not
     (boolean
      (some
       (fn [[x y]]
         (or (= x-min x) (= x-max x)
             (= y-min y) (= y-max y)))
       owned-coords)))))

(defn grid-areas
  [grid]
  (let [grid-coords    (:grid-coords grid)
        all-coords     (:all-coords grid)
        closest-tuples (cp/upfor 4 [grid-coord grid-coords]
                         [grid-coord (closest-coord all-coords grid-coord)])]
    (reduce-kv
     (fn [gdm candidate-coord closest-coords]
       (let [owned-coords (->> (if candidate-coord
                                 (remove #(nil? (second %)) closest-coords)
                                 closest-coords)
                               (mapv first)
                               (sort-by first))
             bounded?     (area-bounded? grid owned-coords)]
         (assoc gdm candidate-coord {:owned-coords owned-coords
                                     :bounded?     bounded?})))
     {}
     (group-by second closest-tuples))))

(def ^:private double-char-range
  (map str/join
       (cmb/cartesian-product (seq "abcdefghijklmnopqrstuvwxyz")
                              (seq "abcdefghijklmnopqrstuvwxyz"))))

(defn ^:private distance-viz
  [grid]
  (let [grid-coords    (:grid-coords grid)
        all-coords     (:all-coords grid)
        rows           (->> (group-by second grid-coords)
                            (sort-by first)
                            (mapv (comp (partial sort-by first) second)))
        closest-lookup (->> (for [grid-coord grid-coords]
                              [grid-coord (closest-coord all-coords grid-coord)])
                            (into {}))
        display-lookup (->> (:all-coords grid)
                            (sort-by first)
                            (map #(vector %2 %1) double-char-range)
                            (into {}))]
    (for [row rows]
      (for [coord row
            :let [closest (get closest-lookup coord)]]
        (cond
          (= closest coord)
          (str/upper-case (get display-lookup closest))

          (nil? closest)
          ".."

          :else
          (get display-lookup closest))))))

(defn find-largest-coord-area
  [grid]
  (let [areas             (grid-areas grid)
        [coord area-data] (->> (filter (fn [[_ area-data]] (:bounded? area-data)) areas)
                               (sort-by (fn [[_ area-data]] (count (:owned-coords area-data))))
                               (last))]
    (count (:owned-coords area-data))))

(defn calculate-largest-coord-area
  [input]
  (let [grid (init-grid (parse-input input))]
    (find-largest-coord-area grid)))

(defn ^:private sum-of-distances
  [comparison-coords basis-coord]
  (->> (map #(man-dist basis-coord %) comparison-coords)
       (apply +)))

(defn find-closest-region-area
  [grid]
  (let [target-coords (:all-coords grid)
        grid-coords   (:grid-coords grid)]
    (->> (pmap #(sum-of-distances target-coords %) grid-coords)
         (filter #(< % 10000))
         (count))))

(defn calculate-closest-region-area
  [input]
  (let [grid (init-grid (parse-input input))]
    (find-closest-region-area grid)))
