(ns aoc-2018.day-11
  (:require [com.climate.claypoole :as cp]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as cmb]
            [clojure.string :as str]
            [taoensso.tufte :as tuf :refer [profile p]]))


(def input
  5177)

(defn ^:private hundreds-digit
  [n]
  (int (mod (/ n 100) 10)))

(defn ^:private power-level
  [serial-number grid-x grid-y]
  (let [rack-id (+ grid-x 10)]
    (-> (* rack-id grid-y)
        (+ serial-number)
        (* rack-id)
        (hundreds-digit)
        (- 5))))

(defn initialize-power-grid
  [serial-number]
  (vec
   (for [y (range 1 301)]
     (vec
      (for [x (range 1 301)]
        (power-level serial-number x y))))))

(defn ^:private cell-level-at
  [grid grid-x grid-y]
  (-> grid
      (nth (dec grid-y))
      (nth (dec grid-x))))

(defn ^:private square-power-at
  [grid square-size grid-x grid-y]
  (let [x (dec grid-x)
        y (dec grid-y)]
    (loop [i      0
           levels []]
      (if (< i square-size)
        (let [row (nth grid (+ y i))]
          (recur (inc i) (into levels (subvec row x (+ x square-size)))))
        (reduce + levels)))))

(defn initialize-sat
  [grid]
  (let [sat-fn  (fn [sat grid-x grid-y]
                  (assoc sat [grid-x grid-y]
                         (+ (cell-level-at grid grid-x grid-y)
                            (get sat [(dec grid-x) grid-y] 0)
                            (get sat [grid-x (dec grid-y)] 0)
                            (- (get sat [(dec grid-x) (dec grid-y)] 0)))))
        sat-map (reduce
                 (fn [sat [grid-x grid-y]]
                   (sat-fn sat grid-x grid-y))
                 {}
                 (cmb/cartesian-product (range 1 301) (range 1 301)))]
    (vec
     (for [y (range 1 301)]
       (vec
        (for [x (range 1 301)]
          (get sat-map [x y])))))))

(defn ^:private get-sat
  [sat sat-x sat-y]
  (if (and (<= 0 sat-x 299)
           (<= 0 sat-y 299))
    (-> sat
        (nth sat-y)
        (nth sat-x))
    0))

(defn ^:private square-power-at-fast
  [sat square-size sat-x sat-y]
  (let [offset (dec square-size)
        d      (get-sat sat (+ sat-x offset) (+ sat-y offset))
        c      (get-sat sat (dec sat-x) (+ sat-y offset))
        b      (get-sat sat (+ sat-x offset) (dec sat-y))
        a      (get-sat sat (dec sat-x) (dec sat-y))]
    (- (+ d a) b c)))

(defn highest-power
  [sat square-size]
  (let [sat-dims (range 0 (- 300 (dec square-size)))]
    (doall
     (reduce
      (fn [prev-highest [sat-x sat-y]]
        (let [prev-highest-power (nth prev-highest 1 Long/MIN_VALUE)
              cur-power          (square-power-at-fast sat
                                                       square-size
                                                       sat-x
                                                       sat-y)]
          (if (>= cur-power prev-highest-power)
            [[(inc sat-x) (inc sat-y)] cur-power]
            prev-highest)))
      []
      (cmb/cartesian-product sat-dims sat-dims)))))

;;;;;;;;;;;;;;;;;;;;;

(defn calculate-highest-3x3-power-coordinate
  [input]
  (let [grid                 (initialize-power-grid input)
        sat                  (initialize-sat grid)
        [[grid-x grid-y] _ ] (highest-power-fast sat 3)]
    (format "%d,%d" grid-x grid-y)))

(defn calculate-highest-power-coordinate-and-size
  [input]
  (let [grid (initialize-power-grid input)
        sat  (initialize-sat grid)]
    (loop [square-size        1
           prev-highest-power [[0 0] Long/MIN_VALUE 0]]
      (let [cur-highest-power (highest-power sat square-size)]
        (if (< (second cur-highest-power) (second prev-highest-power))
          (let [[[highest-x highest-y] _ highest-size] prev-highest-power]
            (format "%d,%d,%d" highest-x highest-y highest-size))
          (recur (inc square-size) (conj cur-highest-power square-size)))))))
