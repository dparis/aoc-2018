(ns aoc-2018.day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.tufte :as tuf :refer [profile p]])
  (:import [java.util
            ArrayDeque
            ArrayList]))


(def input
  (slurp (io/resource "day_9_1_input.txt")))

(def ^:private input-regexp
  #"(\d+) players; last marble is worth (\d+) points")

(defn parse-input
  [input]
  (let [[players turns] (-> (re-find input-regexp input)
                            (rest)
                            (vec))]
    {:players (Integer/parseInt players)
     :turns   (Integer/parseInt turns)}))

(defn ^:private initialize-game-data
  [max-players]
  {:ring           (ArrayList. [0])
   :current-index  0
   :next-marble    1
   :current-player 0
   :max-players    max-players
   :player-scores  {}})

(defn ^:private multiple-of-23?
  [n]
  (= 0 (rem n 23)))

(defn ^:private al-nth
  [#^ArrayList al n]
  (.get al (int n)))

(defn ^:private insert-after!
  [#^ArrayList al n x]
  (p ::insert-after!
     (doto al
       (.add (int (inc n)) x))))

(defn ^:private remove-at!
  [#^ArrayList al n]
  (p ::remove-at!
     (doto al
       (.remove (int n)))))

(defn ^:private add-marble
  [game-data]
  (let [ring           (:ring game-data)
        current-idx    (:current-index game-data)
        current-marble (:next-marble game-data)
        prev-player    (:current-player game-data)
        max-players    (:max-players game-data)
        current-player (inc (mod prev-player max-players))]
    (if (multiple-of-23? current-marble)
      (let [remove-idx     (mod (- current-idx 7) (count ring))
            score-add      (+ current-marble (al-nth ring remove-idx))
            updated-ring   (remove-at! ring remove-idx)
            scores         (:player-scores game-data)
            updated-scores (update scores current-player (fnil + 0) score-add)]
        (assoc game-data
               :ring           updated-ring
               :current-index  remove-idx
               :next-marble    (inc current-marble)
               :current-player current-player
               :player-scores  updated-scores))
      (let [insert-after-index (mod (inc current-idx) (count ring))
            updated-ring       (insert-after! ring
                                              insert-after-index
                                              current-marble)]
        (assoc game-data
               :ring           updated-ring
               :current-index  (inc insert-after-index)
               :next-marble    (inc current-marble)
               :current-player current-player)))))

(defn game
  [max-players]
  (->> (initialize-game-data max-players)
       (iterate add-marble)))

;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private initialize-game-data-fast
  [max-players]
  {:ring           (ArrayDeque. [0])
   :next-marble    1
   :current-player 0
   :max-players    max-players
   :player-scores  {}})

(defn ^:private shift-ring!
  [#^ArrayDeque ring n]
  (if (neg? n)
    (dotimes [_  (Math/abs (int n))] (.addLast ring (.removeFirst ring)))
    (dotimes [_  n]                  (.addFirst ring (.removeLast ring))))
  ring)

(defn ^:private remove-first-from-ring!
  [#^ArrayDeque ring]
  (doto ring (.removeFirst)))

(defn ^:private add-first-to-ring!
  [#^ArrayDeque ring x]
  (doto ring
    (.addFirst x)))

(defn ^:private add-marble-fast
  [game-data]
  (let [ring           (:ring game-data)
        current-marble (:next-marble game-data)
        prev-player    (:current-player game-data)
        max-players    (:max-players game-data)
        current-player (inc (mod prev-player max-players))]
    (if (multiple-of-23? current-marble)
      (let [updated-ring   (shift-ring! ring -7)
            scores         (:player-scores game-data)
            score-add      (+ current-marble (first updated-ring))
            updated-scores (update scores current-player (fnil + 0) score-add)]
        (assoc game-data
               :ring           (-> (remove-first-from-ring! updated-ring)
                                   (shift-ring! 1))
               :next-marble    (inc current-marble)
               :current-player current-player
               :player-scores  updated-scores))

      (let [updated-ring (-> (shift-ring! ring 1)
                             (add-first-to-ring! current-marble))]
        (assoc game-data
               :ring           updated-ring
               :next-marble    (inc current-marble)
               :current-player current-player)))))

(defn game-fast
  [max-players]
  (->> (initialize-game-data-fast max-players)
       (iterate add-marble-fast)))

(defn highest-after-turns
  [game n]
  (let [result (last (take (inc n) game))
        scores (:player-scores result)]
    (->> (sort-by last scores)
         (last))))

(defn calculate-winning-score
  [input]
  (let [{:keys [players turns]} (parse-input input)]
    (-> (game-fast players)
        (highest-after-turns turns)
        (last))))

(defn calculate-winning-score-100x
  [input]
  (let [{:keys [players turns]} (parse-input input)]
    (-> (game-fast players)
        (highest-after-turns (* 100 turns))
        (last))))
