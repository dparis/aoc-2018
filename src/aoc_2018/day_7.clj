(ns aoc-2018.day-7
  (:require [clojure.java.io :as io]
            [loom.alg :as ga]
            [loom.alg-generic :as gag]
            [loom.graph :as g]
            [loom.io :as gio]
            [clojure.set :refer [difference union intersection]]
            [clojure.string :as str]))


(def input
  (slurp (io/resource "day_7_1_input.txt")))

(def ^:private step-regexp
  #"Step ([A-Z]) must be finished before step ([A-Z]) can begin.")

(defn parse-input
  [input]
  (let [lines (str/split input #"\n")]
    (for [line lines
          :let [[_ dependency target] (re-find step-regexp line)]]
      {:dependency dependency
       :target     target})))

(defn init-graph
  [steps]
  (->> (mapv (juxt :dependency :target) steps)
       (apply g/digraph)))

(defn view-steps
  [graph]
  (gio/view graph))

(defn step-order
  [graph available-nodes-fn work-fn]
  (loop [remaining-graph graph
         steps           []]
    (let [available-nodes (available-nodes-fn remaining-graph)
          current-nodes   (work-fn available-nodes)
          updated-graph   (apply g/remove-nodes remaining-graph current-nodes)]
      (cond
        (and (empty? available-nodes) (not (empty? (g/nodes updated-graph))))
        nil

        (empty? available-nodes)
        (str/join steps)

        :else
        (recur updated-graph (into steps current-nodes))))))

(defn all-minimal-nodes
  [graph]
  (let [nodes (g/nodes graph)]
    (filter #(= 0 (g/in-degree graph %)) nodes)))

(defn build-all-until-first-lex
  []
  (let [first-time?_ (atom true)]
    (fn [nodes]
      (if @first-time?_
        (do
          (reset! first-time?_ false)
          (vec (sort nodes)))
        (some-> (sort nodes)
                (first)
                (vector))))))

(defn calculate-step-order
  [input]
  (let [graph   (init-graph (parse-input input))
        work-fn (build-all-until-first-lex)]
    (step-order graph all-minimal-nodes work-fn)))

(defn ^:private init-workers
  [n]
  (reduce
   (fn [m id]
     (assoc m id {:id             id
                  :current        nil
                  :remaining-iter 0}))
   {}
   (range n)))

(defn ^:private worker-state-completed?
  [state]
  (and (:current state) (= 0 (:remaining-iter state))))

(defn ^:private completed-work
  [workers]
  (->> (filter (fn [[_ state]] (worker-state-completed? state)) workers)
       (map (fn [[_ state]] (:current state)))
       (sort)
       (vec)))

(defn ^:private decrement-active-workers
  [workers]
  (->> (for [[id state] workers
             :let [remaining-iter (:remaining-iter state)]]
         (if (> remaining-iter 0)
           (vector id (update state :remaining-iter dec))
           (vector id state)))
       (into {})))

(defn ^:private reset-completed-workers
  [workers]
  (->> (for [[id state] workers
             :let [current        (:current state)
                   remaining-iter (:remaining-iter state)]]
         (if (and (= remaining-iter 0) (not (nil? current)))
           (vector id (assoc state :current nil))
           (vector id state)))
       (into {})))

(defn ^:private available-worker-ids
  [workers]
  (->> (filter (fn [[_ state]] (= 0 (:remaining-iter state))) workers)
       (map first)
       (set)))

(defn ^:private active-nodes
  [workers]
  (->> (map (fn [[_ state]] (:current state)) workers)
       (remove nil?)
       (set)))

(defn ^:private node-cost
  [cost-factor]
  (+ 60 cost-factor))

(defn ^:private build-node-work-lookup
  [cost-fn]
  (->> (str/split "ABCDEFGHIJKLMNOPQRSTUVWXYZ" #"")
       (map (fn [n node] [node (cost-fn (inc n))]) (range))
       (into {})))

(defn ^:private claim-nodes
  [workers nodes node-work-lookup]
  (loop [updated-workers workers
         available-ids   (available-worker-ids workers)
         remaining-nodes (->> (active-nodes workers)
                              (apply disj (set nodes))
                              (sort-by node-work-lookup))]
    (let [[id & ids] available-ids
          [n & ns]   remaining-nodes]
      (if (or (empty? available-ids) (empty? remaining-nodes))
        updated-workers
        (recur (-> updated-workers
                   (assoc-in [id :current] n)
                   (assoc-in [id :remaining-iter] (get node-work-lookup n)))
               ids
               ns)))))

(defn update-workers
  [workers nodes node-work-lookup]
  (-> (reset-completed-workers workers)
      (claim-nodes nodes node-work-lookup)
      (decrement-active-workers)))

(defn build-parallel-with-lex-cost
  ([num-workers]
   (build-parallel-with-lex-cost num-workers (atom -1)))
  ([num-workers time_]
   (build-parallel-with-lex-cost num-workers time_ node-cost))
  ([num-workers time_ cost-fn]
   (let [workers_  (atom (init-workers num-workers))
         nw-lookup (build-node-work-lookup cost-fn)]
     (fn [nodes]
       (swap! workers_ update-workers nodes nw-lookup)
       (swap! time_ inc)
       (let [completed (completed-work @workers_)]
         completed)))))

(defn calculate-parallel-step-order-seconds
  [input]
  (let [graph   (init-graph (parse-input input))
        time_   (atom -1)
        work-fn (build-parallel-with-lex-cost 5 time_ node-cost)
        steps   (step-order graph all-minimal-nodes work-fn)]
    @time_))
