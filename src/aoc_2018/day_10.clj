(ns aoc-2018.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.awt
            Color
            FlowLayout]
           [java.awt.image
            BufferedImage]
           [javax.imageio
            ImageIO]
           [javax.swing
            ImageIcon
            JFrame
            JLabel]))


(def input
  (slurp (io/resource "day_10_1_input.txt")))

(def ^:private line-regexp
  #"\s*position=<([-\d\s]+),\s*([-\d\s]+)>\s*velocity=<([-\d\s]+),\s*([-\d\s]+)>\s*")

(defn ^:private parse-value
  [s]
  (Integer/parseInt (str/trim s)))

(defn parse-input
  [input]
  (for [[i line] (partition-all 2 (interleave (range) (str/split input #"\n")))
        :let [[_ px py vx vy] (re-find line-regexp line)]]
    {:id         i
     :position-x (parse-value px)
     :position-y (parse-value py)
     :velocity-x (parse-value vx)
     :velocity-y (parse-value vy)}))

(defn ^:private calculate-extents
  [min-x min-y max-x max-y]
  {:min-x    min-x
   :min-y    min-y
   :max-x    max-x
   :max-y    max-y
   :offset-x (Math/abs (int min-x))
   :offset-y (Math/abs (int min-y))
   :width    (+ (Math/abs (int min-x))
                (Math/abs (int max-x)))
   :height   (+ (Math/abs (int min-y))
                (Math/abs (int max-y)))})

(defn ^:private beacon-extents
  [beacons]
  (loop [min-x             0
         min-y             0
         max-x             0
         max-y             0
         remaining-beacons beacons]
    (let [[b & bs] remaining-beacons]
      (if (nil? b)
        (calculate-extents min-x min-y max-x max-y)
        (recur (int (min min-x (:position-x b)))
               (int (min min-y (:position-y b)))
               (int (max max-x (:position-x b)))
               (int (max max-y (:position-y b)))
               bs)))))

(defn ^:private normalize-beacon
  [offset-x offset-y beacon]
  (-> beacon
      (update :position-x + offset-x)
      (update :position-y + offset-y)))

(defn ^:private progress-beacons
  [beacons n]
  (->> (for [beacon beacons
             :let [{:keys [velocity-x velocity-y]} beacon]]
         (-> beacon
             (update :position-x + (* velocity-x n))
             (update :position-y + (* velocity-y n))))
       (sort-by (juxt :position-y :position-x))))

(defn search-for-message
  [beacons]
  (loop [n                0
         previous-beacons []
         previous-area    Long/MAX_VALUE]
    (let [progressed-beacons (progress-beacons beacons n)
          extents            (beacon-extents progressed-beacons)
          area               (long (* (:width extents) (:height extents)))]
      (if (> area previous-area)
        {:seconds (dec n)
         :beacons previous-beacons}
        (recur (inc n) progressed-beacons area)))))

(defn ^:private render-beacons-shell!
  [extents positions]
  (let [double-width  (* 2 (:width extents))
        double-height (* 2 (:height extents))]
    (doseq [y (range double-height)]
      (do (doseq [x    (range double-width)
                  :let [icon (if (contains? positions [x y]) "*" ".")]]
            (print icon))
          (println)))))

(defn ^:prviate render-beacons-png!
  [extents positions]
  (let [block         2
        double-width  (* 2 (:width extents))
        double-height (* 2 (:height extents))
        image         (BufferedImage.
                       (int (* block double-width))
                       (int (* block double-height))
                       BufferedImage/TYPE_INT_ARGB)
        graphics      (.createGraphics image)]
    (doto graphics
      (.setColor Color/BLACK)
      (.fillRect 0 0 (int (* block double-width)) (int (* block double-height))))

    (doseq [y (range double-height)
            x (range double-width)]
      (when (contains? positions [x y])
        (doto graphics
          (.setColor Color/RED)
          (.fillRect (* block x) (* block y) block block))))

    (let [output (io/file "resources/day_10_1_message.png")]
      (.createNewFile output)
      (ImageIO/write image "png" output))))

(defn ^:private render-beacons-window!
  [extents positions]
  (let [block         2
        double-width  (* 2 (:width extents))
        double-height (* 2 (:height extents))
        image         (BufferedImage.
                       (int (* block double-width))
                       (int (* block double-height))
                       BufferedImage/TYPE_INT_ARGB)
        graphics      (.createGraphics image)
        frame         (JFrame.)
        pane          (.getContentPane frame)]
    (doto graphics
      (.setColor Color/BLACK)
      (.fillRect 0 0 (int (* block double-width)) (int (* block double-height))))

    (doseq [y (range double-height)
            x (range double-width)]
      (when (contains? positions [x y])
        (doto graphics
          (.setColor Color/RED)
          (.fillRect (* block x) (* block y) block block))))

    (doto pane
      (.setLayout (FlowLayout.))
      (.add (JLabel. (ImageIcon. image))))

    (doto frame
      (.pack)
      (.setVisible true))))

(defn ^:private render-beacons!
  [beacons output-opt]
  (let [extents            (beacon-extents beacons)
        offset-x           (:offset-x extents)
        offset-y           (:offset-y extents)
        normalize-fn       (partial normalize-beacon offset-x offset-y)
        normalized-beacons (map normalize-fn beacons)
        positions          (set (map (juxt :position-x :position-y)
                                     normalized-beacons))]
    (case output-opt
      :window (render-beacons-window extents positions)
      :png    (render-beacons-png extents positions)
      :shell  (render-beacons-shell extents positions))))

(defn calculate-message!
  [input]
  (let [beacons      (parse-input input)
        message-data (search-for-message beacons)]
    (render-beacons (:beacons message-data) :png)

    ;; Image is written to disk at resources/day_10_1_message.png to verify
    (vector "LCPGPXGL" (:seconds message-data))))
