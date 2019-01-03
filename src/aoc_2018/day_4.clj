(ns aoc-2018.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [java-time :as t]
            [java-time.repl :as trepl]))


(def input
  (slurp (io/resource "day_4_1_input.txt")))

(def ^:private log-line-regexp
  #"\s*\[(.*)\]\s+(.*)")

(def ^:private timestamp-format
  "yyyy-MM-dd HH:mm")

(def ^:private falls-asleep-regexp
  #"falls asleep")

(def ^:private wakes-up-regexp
  #"wakes up")

(def ^:private begins-shift-regexp
  #"Guard \#(\d+) begins shift")

(defn ^:private parse-log-text
  [log-text]
  (cond
    (re-find falls-asleep-regexp log-text)
    {:event-type :falls-asleep}

    (re-find wakes-up-regexp log-text)
    {:event-type :wakes-up}

    :else
    (let [guard-id (second (re-find begins-shift-regexp log-text))]
      {:event-type :begins-shift
       :guard-id   (Integer/parseInt guard-id)})))

(defn parse-input
  [input]
  (let [lines (str/split input #"\n")]
    (for [line lines
          :let [[_ timestamp log-text] (re-find log-line-regexp line)
                datetime               (t/local-date-time
                                        timestamp-format
                                        timestamp)
                log-event              (parse-log-text log-text)]]
      (assoc log-event :timestamp datetime))))

(def ^:private path-lookup
  {:begins-shift :shifts
   :falls-asleep :sleep-starts
   :wakes-up     :sleep-ends})

(defn ^:private update-guard-data
  [guard-data guard-id event]
  (let [path      (get path-lookup (:event-type event))
        timestamp (:timestamp event)]
    (update-in guard-data [guard-id path] (fnil conj []) timestamp)))

(defn ^:private sleep-minutes
  [sleep]
  (let [[start end] sleep
        minutes     (-> (java.time.Duration/between start end)
                        (.toMinutes))]
    (map #(t/plus start (t/minutes %)) (range minutes))))

(defn ^:private format-guard-data
  [guard-data]
  (->> (for [[guard-id data] guard-data
             :let [sleep-starts      (:sleep-starts data)
                   sleep-ends        (:sleep-ends data)
                   sleep-minutes-set (->> (interleave sleep-starts sleep-ends)
                                          (partition-all 2)
                                          (map sleep-minutes)
                                          (flatten)
                                          (set))]]
         [guard-id (assoc data :sleep-minutes sleep-minutes-set)])
       (into {})))

(defn compile-guard-data
  [log-events]
  (loop [remaining-events   (sort-by :timestamp log-events)
         last-guard-id-seen nil
         guard-data         {}]
    (if-let [event (first remaining-events)]
      (let [current-guard-id (or (:guard-id event) last-guard-id-seen)]
        (recur (next remaining-events)
               current-guard-id
               (update-guard-data guard-data current-guard-id event)))
      (format-guard-data guard-data))))

(defn ^:private find-sleepiest-guard
  [guard-data]
  (->> guard-data
       (map (fn [[id data]] [id (count (:sleep-minutes data))]))
       (sort-by second)
       (last)))

(defn ^:private most-slept-minute
  [guard-data guard-id]
  (let [minutes (get-in guard-data [guard-id :sleep-minutes])]
    (->> (map #(t/as % :minute-of-day) minutes)
         (frequencies)
         (sort-by second)
         (last)
         (first))))

(defn find-guard-minute
  [log-events]
  (let [guard-data         (compile-guard-data log-events)
        sleepiest-guard-id (first (find-sleepiest-guard guard-data))
        sleepiest-minute   (most-slept-minute guard-data sleepiest-guard-id)]
    (* sleepiest-guard-id sleepiest-minute)))

(defn calculate-guard-minute
  [input]
  (let [log-events (parse-input input)]
    (find-guard-minute log-events)))

(defn minute-guard-lookup
  [guard-data]
  (reduce-kv
   (fn [lookup guard-id data]
     (->> (reduce
           (fn [inner-lookup [minute-of-day guard-id]]
             (update inner-lookup minute-of-day (fnil conj []) guard-id))
           {}
           (for [minute (:sleep-minutes data)]
             [(t/as minute :minute-of-day) guard-id]))
          (merge-with into lookup)))
   {}
   guard-data))

(defn find-minute-guard
  [log-events]
  (let [guard-data      (compile-guard-data log-events)
        mg-lookup       (minute-guard-lookup guard-data)
        freqency-lookup (reduce-kv (fn [m k v]
                                     (assoc m k (frequencies v)))
                                   {}
                                   mg-lookup)]
    (->> (loop [remaining-lookup  freqency-lookup
                highest-frequency 0
                guard-id          nil
                sleepiest-minute  nil]
           (if-let [[minute-of-day freqs] (first remaining-lookup)]
             (let [[cur-guard-id cur-frequency] (last (sort-by second freqs))]
               (if (> cur-frequency highest-frequency)
                 (recur (next remaining-lookup)
                        cur-frequency
                        cur-guard-id
                        minute-of-day)
                 (recur (next remaining-lookup)
                        highest-frequency
                        guard-id
                        sleepiest-minute)))
             [guard-id sleepiest-minute]))
         (apply *))))

(defn calculate-minute-guard
  [input]
  (let [log-events (parse-input input)]
    (find-minute-guard log-events)))
