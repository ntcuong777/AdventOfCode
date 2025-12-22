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

(defn find-first-vert [vert-lines x y direction]
  (let [lines (case direction
                :left vert-lines ;; assuming sorted low-to-high x
                :right (reverse vert-lines))
        default-if-none (case direction
                          :left 0
                          :right Long/MAX_VALUE)
        found-line (->> lines
                        (filter (fn [{vx :x vy-range :y}]
                                  (and (< vx x)
                                       (<= (inc (first vy-range))
                                           y
                                           (second vy-range)))))
                        last)]
    (if (nil? found-line)
      default-if-none
      (:x found-line))))

(def debug? false)

(defmacro spy [label & body]
  (if debug?
    `(let [result# (do ~@body)]
       (println ~label ":" result#)
       result#)
    `(do ~@body)))

(defn lines-to-points [lines]
  (apply set/union
         (map (fn [{x-maybe-range :x y-maybe-range :y}]
                (set (for [x (if (seqable? x-maybe-range)
                               x-maybe-range
                               [x-maybe-range])
                           y (if (seqable? y-maybe-range)
                               y-maybe-range
                               [y-maybe-range])]
                       [x y])))
              lines)))

(defn get-max-y-for-x [horiz-lines x & {:keys [default-if-none]
                                        :or {default-if-none 0}}]
  (let [relevant-lines (filter (fn [{x-range :x}]
                                 (and (<= (first x-range) x)
                                      (>= (second x-range) x)))
                               horiz-lines)
        max-y-line (when (seq relevant-lines)
                     (apply max-key
                            (fn [{y :y}] y)
                            relevant-lines))]
    (if (nil? max-y-line)
      default-if-none
      (:y max-y-line))))

(defn get-max-y-for-xs [horiz-lines xs & {:keys [default-if-none]
                                          :or {default-if-none 0}}]
  (or (apply merge
             (map (fn [x]
                    (let [max-y (get-max-y-for-x horiz-lines
                                                 x
                                                 :default-if-none default-if-none)]
                      {x max-y}))
                  xs))
      {}))

(defn max-y-in-range [max-y-per-xs x-range]
  (let [[min-x max-x] [(apply min x-range)
                       (apply max x-range)]
        ys (map (fn [[x y]]
                  (if (<= min-x x max-x)
                    y ;; some y can be nil if no lines exist at that x
                    0))
                max-y-per-xs)]
    (if (and (seq ys)
             (every? (complement nil?) ys))
      (apply max ys)
      nil)))

(defn pair-point-and-get-max-area [pt other-pts max-y-per-xs x-bounds]
  (let [[px py] pt
        [min-x max-x] x-bounds
        valid-pts (filter (fn [[ox oy]]
                            (and (<= min-x ox max-x)
                                 (<= oy py)
                                 (= oy (max-y-per-xs ox))))
                          other-pts)
        valid-areas (for [[ox oy] valid-pts
                          :let [max-y (max-y-in-range max-y-per-xs [px ox])]
                          :when (or (and (not (nil? max-y))
                                         (= max-y oy))
                                    (and (nil? max-y)
                                         (= oy py)))
                          :let [area-val (area pt [ox oy])]]
                      area-val)]
    (if (seq valid-areas)
      (apply max valid-areas)
      0)))

(defn find-max-rect-of-line
  "Assuming lines are pre-sorted appropriately and horiz-line is traversed
in low-to-high y order"
  [horiz-line all-vert-lines all-horiz-lines]
  (let [{hy :y} horiz-line
        anchor-pts (map (fn [[x y]]
                          {:point [x y]
                           :x-boundary [(find-first-vert all-vert-lines
                                                         x
                                                         hy
                                                         :left)
                                        (find-first-vert all-vert-lines
                                                         x
                                                         hy
                                                         :right)]})
                        (lines-to-points [horiz-line]))
        eq-horiz-lines (spy "eq-horiz-lines"
                            (filter (fn [{ly :y}]
                                      (= ly hy))
                                    all-horiz-lines))
        eq-horiz-points (spy "eq-horiz-points"
                             (lines-to-points eq-horiz-lines))
        lower-horiz-lines (spy "lower-horiz-lines"
                               (filter (fn [{ly :y}]
                                         (< ly hy))
                                       all-horiz-lines))
        lower-horiz-points (spy "lower-horiz-points"
                                (lines-to-points lower-horiz-lines))
        considering-xs (spy "considering-xs"
                            (apply set/union (map (fn [{x-range :x}]
                                                    (set x-range))
                                                  lower-horiz-lines)))
        xs-with-max-y (spy "xs-with-max-y"
                           (get-max-y-for-xs lower-horiz-lines
                                             considering-xs
                                             :default-if-none nil))
        max-area-lower-pts (spy "max-area-lower-pts"
                                (for [{[x y] :point x-range :x-boundary} anchor-pts]
                                  (pair-point-and-get-max-area [x y]
                                                               lower-horiz-points
                                                               (if (contains? xs-with-max-y x)
                                                                 xs-with-max-y
                                                                 (assoc xs-with-max-y x y))
                                                               x-range)))
        final-max-lower (if (seq max-area-lower-pts)
                          (apply max max-area-lower-pts)
                          0)

        xs-with-max-y-eq (spy "xs-with-max-y-eq"
                              (get-max-y-for-xs eq-horiz-lines
                                                (apply set/union (map (fn [{x-range :x}]
                                                                        (set x-range))
                                                                      eq-horiz-lines))
                                                :default-if-none hy))
        max-area-eq-pts (spy "max-area-eq-pts"
                             (for [pt anchor-pts]
                               (pair-point-and-get-max-area (:point pt)
                                                            eq-horiz-points
                                                            xs-with-max-y-eq
                                                            (:x-boundary pt))))

        final-max-eq (if (seq max-area-eq-pts)
                       (apply max max-area-eq-pts)
                       0)]
    (max final-max-lower final-max-eq)))

(defn largest-red-green-rect [points] ;
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
    (if (seq maxes)
      (apply max maxes)
      0)))

(def sample-input (parse-input "2025/day9_sample.txt"))
(def real-input (parse-input "2025/day9_input.txt"))

(def part1-sample-brute (largest-rect-brute sample-input))
(def part1-real-brute (largest-rect-brute real-input))

(def part2-sample-red-green (largest-red-green-rect sample-input))
(def part2-real-red-green (largest-red-green-rect real-input))
