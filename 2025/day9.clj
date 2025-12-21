(ns day9
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [inp-file]
  (->> (slurp inp-file)
       str/split-lines
       (map #(let [[x y] (vec (str/split % #","))]
               [(Long/parseLong x) (Long/parseLong y)]))))

(defn area [[x1 y1] [x2 y2]]
  (let [width (inc (Math/abs (- x2 x1)))
        height (inc (Math/abs (- y2 y1)))]
    (* width height)))

(defn largest-rect-brute [points]
  (let [point-pairs (for [p1 points
                          p2 points
                          :when (not= p1 p2)]
                      [p1 p2])
        valid-areas (map (fn [[p1 p2]]
                           (area p1 p2))
                         point-pairs)]
    (apply max valid-areas)))

(defn rect-corners [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2
        min-x (min x1 x2)
        max-x (max x1 x2)
        min-y (min y1 y2)
        max-y (max y1 y2)]
    [[min-x min-y]
     [min-x max-y]
     [max-x min-y]
     [max-x max-y]]))

(defn rect-edges [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2
        min-x (min x1 x2)
        max-x (max x1 x2)
        min-y (min y1 y2)
        max-y (max y1 y2)]
    {:left [[min-x min-y] [min-x max-y]]   ; left edge
     :top [[min-x max-y] [max-x max-y]]   ; top edge
     :right [[max-x max-y] [max-x min-y]]   ; right edge
     :bottom [[max-x min-y] [min-x min-y]]})) ; bottom edge

(defn find-first-vert-left [vert-lines x y]
  (let [found-line (->> vert-lines
                        (filter (fn [{vx :x vy-range :y}]
                                  (and (< vx x)
                                       (<= (inc (first vy-range))
                                           y
                                           (second vy-range)))))
                        last)]
    (if (nil? found-line)
      0
      (:x found-line))))

(defn find-first-vert-right [vert-lines x y]
  (let [found-value (->> vert-lines
                         (filter (fn [{vx :x vy-range :y}]
                                   (and (> vx x)
                                        (<= (inc (first vy-range))
                                            y
                                            (second vy-range)))))
                         first)]
    (if (nil? found-value)
      Long/MAX_VALUE
      (:x found-value))))

(def debug? true)

(defmacro spy [label & body]
  (if debug?
    `(let [result# (do ~@body)]
       (println ~label ":" result#)
       result#)
    `(do ~@body)))

(defn find-max-rect-of-line
  "Assuming lines are pre-sorted appropriately and horiz-line is traversed
in low-to-high y order"
  [horiz-line all-vert-lines all-horiz-lines]
  (println "Considering horiz line:" horiz-line)
  (let [{hy :y hx-range :x} horiz-line
        [cur-line-x1 cur-line-x2] hx-range
        hx-left-verts (spy "hx-left-verts"
                           (map #(find-first-vert-left all-vert-lines
                                                       %
                                                       hy)
                                hx-range))
        hx-right-verts (spy "hx-right-verts"
                            (map #(find-first-vert-right all-vert-lines
                                                         %
                                                         hy)
                                 hx-range))
        eq-horiz-lines (spy "eq-horiz-lines"
                            (filter (fn [{ly :y}]
                                      (= ly hy))
                                    all-horiz-lines))
        lower-horiz-lines (spy "lower-horiz-lines"
                               (filter (fn [{ly :y}]
                                         (< ly hy))
                                       all-horiz-lines))
        considering-xs (spy "considering-xs"
                            (apply set/union (map (fn [{x-range :x}]
                                                    (set x-range))
                                                  lower-horiz-lines)))
        xs-with-max-y (spy "xs-with-max-y"
                           (apply merge (map (fn [x]
                                               (let [max-y (->> lower-horiz-lines
                                                                (filter (fn [{x-range :x}]
                                                                          (and (<= (first x-range) x)
                                                                               (>= (second x-range) x))))
                                                                last
                                                                :y)]
                                                 {x max-y}))
                                             considering-xs)))
        maxes-lower (spy "maxes-lower"
                         (for [line lower-horiz-lines
                               :when (seq xs-with-max-y)
                               :let [line-y (:y line)
                                     line-x-range (spy "line-x-range" (:x line))
                                     valid-xs-wrt-cur-x1 (spy "valid-xs-wrt-cur-x1"
                                                              (filter (fn [x]
                                                                    ;; only consider x-points of the line
                                                                    ;; where it does not exceed the vertical bounds
                                                                    ;; and the highest horiz bar is the same as the current line
                                                                        (and (<= (first hx-left-verts)
                                                                                 x
                                                                                 (first hx-right-verts))
                                                                             (= line-y (xs-with-max-y x))))
                                                                      (:x line)))
                                     valid-xs-wrt-cur-x2 (spy "valid-xs-wrt-cur-x2"
                                                              (filter (fn [x]
                                                                        (and (<= (second hx-left-verts)
                                                                                 x
                                                                                 (second hx-right-verts))
                                                                             (= line-y (xs-with-max-y x))))
                                                                      line-x-range))]
                               :when (or (not (empty? valid-xs-wrt-cur-x1))
                                         (not (empty? valid-xs-wrt-cur-x2)))]
                           (let [areas-1 (if (not (empty? valid-xs-wrt-cur-x1))
                                           (map (fn [x]
                                                  (area [x line-y]
                                                        [cur-line-x1 hy]))
                                                valid-xs-wrt-cur-x1)
                                           [])
                                 max-areas-1 (if (not (empty? areas-1))
                                               (apply max areas-1)
                                               0)
                                 areas-2 (if (not (empty? valid-xs-wrt-cur-x2))
                                           (map (fn [x]
                                                  (area [x line-y]
                                                        [cur-line-x2 hy]))
                                                valid-xs-wrt-cur-x2)
                                           [])
                                 max-areas-2 (if (not (empty? areas-2))
                                               (apply max areas-2)
                                               0)]
                             (max max-areas-1 max-areas-2))))
        final-max-lower (if (seq maxes-lower)
                          (apply max maxes-lower)
                          0)

        maxes-eq (spy "max-eq"
                      (for [line eq-horiz-lines
                            :let [line-x-range (:x line)
                                  valid-xs-wrt-cur-x1 (filter (fn [x]
                                                                (<= (first hx-left-verts)
                                                                    x
                                                                    (first hx-right-verts)))
                                                              line-x-range)
                                  valid-xs-wrt-second (filter (fn [x]
                                                                (<= (second hx-left-verts)
                                                                    x
                                                                    (second hx-right-verts)))
                                                              line-x-range)]
                            :when (or (not (empty? valid-xs-wrt-cur-x1))
                                      (not (empty? valid-xs-wrt-second)))]
                        (let [areas-1 (if (not (empty? valid-xs-wrt-cur-x1))
                                        (map (fn [x]
                                               (area [x hy]
                                                     [cur-line-x1 hy]))
                                             valid-xs-wrt-cur-x1)
                                        [])
                              max-areas-1 (if (not (empty? areas-1))
                                            (apply max areas-1)
                                            0)
                              areas-2 (if (not (empty? valid-xs-wrt-second))
                                        (map (fn [x]
                                               (area [x hy]
                                                     [cur-line-x2 hy]))
                                             valid-xs-wrt-second)
                                        [])
                              max-areas-2 (if (not (empty? areas-2))
                                            (apply max areas-2)
                                            0)]
                          (max max-areas-1 max-areas-2))))
        final-max-eq (if (seq maxes-eq)
                       (apply max maxes-eq)
                       0)]
    (max final-max-lower final-max-eq)))

(defn largest-red-green-rect [points]
  (let [lines (map #(vector %1 %2)
                   points
                   (conj (vec (rest points)) (first points)))
        vert-lines (sort-by #(vector (:x %) (:y %))
                            (->> lines
                                 (filter (fn [[[x1 _] [x2 _]]]
                                           (= x1 x2)))
                                 (map (fn [[[lx1 ly1] [_ ly2]]]
                                        {:dir :vertical
                                         :x lx1
                                         :y [(min ly1 ly2) (max ly1 ly2)]}))))
        horiz-lines (sort-by #(vector (:y %) (:x %))
                             (->> lines
                                  (filter (fn [[[_ y1] [_ y2]]]
                                            (= y1 y2)))
                                  (map (fn [[[lx1 ly1] [lx2 _]]]
                                         {:dir :horizontal
                                          :y ly1
                                          :x [(min lx1 lx2) (max lx1 lx2)]}))))
        maxes (map (fn [horiz-line]
                     (find-max-rect-of-line horiz-line
                                            vert-lines
                                            horiz-lines))
                   horiz-lines)]
    (println "Maxes found for all horiz lines:" (doall maxes))
    (if (seq maxes)
      (apply max maxes)
      0)))

(def sample-input (parse-input "2025/day9_sample.txt"))
(def real-input (parse-input "2025/day9_input.txt"))

(def part1-sample-brute (largest-rect-brute sample-input))
(def part1-real-brute (largest-rect-brute real-input))

(def part2-sample-red-green (largest-red-green-rect sample-input))
(def part2-real-red-green (largest-red-green-rect real-input))
