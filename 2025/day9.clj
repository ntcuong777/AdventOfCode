(ns day9
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input
  [inp-file]
  (->> (slurp inp-file)
       str/split-lines
       (map #(let [[x y] (vec (str/split % #","))]
               [(Long/parseLong x) (Long/parseLong y)]))))

(defn area
  [[x1 y1] [x2 y2]]
  (let [width (inc (Math/abs (- x2 x1)))
        height (inc (Math/abs (- y2 y1)))]
    (* width height)))

(defn largest-rect-brute
  [points]
  (let [point-pairs (for [p1 points
                          p2 points
                          :when (not= p1 p2)]
                      [p1 p2])
        valid-areas (map (fn [[p1 p2]]
                           (area p1 p2))
                         point-pairs)]
    (apply max valid-areas)))

(defn find-first-vert
  [vert-lines x y direction]
  (let [lines (if (= direction :left)
                vert-lines
                (reverse vert-lines))
        default-if-none (case direction
                          :left 0
                          :right Long/MAX_VALUE)
        x-filter (fn [vx]
                   (case direction
                     :left (<= vx x)
                     :right (> vx x)))
        found-line (->> lines
                        (filter (fn [{vx :x vy-range :y}]
                                  (let [min-y (apply min vy-range)
                                        max-y (apply max vy-range)]
                                    (and (x-filter vx)
                                         (<= (inc min-y)
                                             y
                                             max-y)))))
                        last)]
    ;; (println default-if-none "for" direction "at x" x "y" y "found line:" found-line)
    (if (nil? found-line)
      default-if-none
      (:x found-line))))

(def debug? false)

(defmacro spy
  [label & body]
  (if debug?
    `(let [result# (do ~@body)]
       (println ~label ":" result#)
       result#)
    `(do ~@body)))

(defn lines-to-points
  [lines]
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

(defn get-max-y-for-x
  [horiz-lines x & {:keys [default-if-none]
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

(defn get-max-y-for-xs
  [horiz-lines xs & {:keys [default-if-none]
                     :or {default-if-none 0}}]
  (or (apply merge
             (map (fn [x]
                    (let [max-y (get-max-y-for-x horiz-lines
                                                 x
                                                 :default-if-none default-if-none)]
                      {x max-y}))
                  xs))
      {}))

(defn max-y-in-range
  [max-y-per-xs x-range]
  (let [[min-x max-x] [(apply min x-range)
                       (apply max x-range)]
        ys (map (fn [[x y]]
                  (if (<= min-x x max-x)
                    y ; some y can be nil if no lines exist at that x
                    0))
                max-y-per-xs)]
    (if (and (seq ys)
             (every? (complement nil?) ys))
      (apply max ys)
      nil)))

(defn pair-point-and-get-max-area
  [pt other-pts max-y-per-xs x-bounds]
  ;; (println "Pairing point" pt " with " other-pts " and x-bounds " x-bounds " where max-y-per-xs is " max-y-per-xs)
  (let [[px py] pt
        [min-x max-x] x-bounds
        valid-pts (filter (fn [[ox oy]]
                            (and (<= min-x ox max-x)
                                 (<= oy py)
                                 (= oy (max-y-per-xs ox))))
                          other-pts)
        valid-areas (doall (for [[ox oy] valid-pts
                                 :let [max-y (max-y-in-range max-y-per-xs [px ox])]
                                 :when (or (and (not (nil? max-y))
                                                (<= max-y oy))
                                           (and (nil? max-y)
                                                (= oy py)))
                                 :let [area-val (area pt [ox oy])]]
                             area-val))]
    (if (seq valid-areas)
      (apply max valid-areas)
      0)))

(defn find-max-rect-of-line
  "Assuming lines are pre-sorted appropriately and horiz-line is traversed
in low-to-high y order"
  [horiz-line all-vert-lines all-horiz-lines]
  ;; (println "Considering horiz line:" horiz-line " with vert lines:" all-vert-lines " and horiz lines:" all-horiz-lines)
  (let [{hy :y} horiz-line
        anchor-pts (spy "anchor-pts"
                        (map (fn [[x y]]
                               {:point [x y]
                                :x-boundary [(find-first-vert all-vert-lines
                                                              x
                                                              hy
                                                              :left)
                                             (find-first-vert all-vert-lines
                                                              x
                                                              hy
                                                              :right)]})
                             (lines-to-points [horiz-line])))
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
                                                  (conj lower-horiz-lines horiz-line))))
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
    ;; (println "Max lower:" final-max-lower " Max eq:" final-max-eq)
    (max final-max-lower final-max-eq)))

(defn make-point
  [x y]
  {:x x
   :y y})

(defn points-as-map [vec-points]
  (vec (map (fn [[x y]]
              (make-point x y))
            vec-points)))

(defn make-line
  [p1 p2]
  (let [orientation (if (= (first p1) (first p2))
                      :vertical
                      :horizontal)
        points (if (= orientation :vertical)
                 (vec (sort-by :y [p1 p2]))
                 (vec (sort-by :x [p1 p2])))]
    {:orientation orientation
     :points points}))

(defn lines-from-points [points]
  (let [points-map (points-as-map points)]
    (map make-line
         points-map
         (conj (vec (rest points-map)) (first points-map)))))

(defn intersecting-point?
  "Return the midpoint of intersection if lines intersect, else nil"
  [line1 line2]
  (let [{l1-orientation :orientation
         [l1p1 l1p2] :points} line1
        {l2-orientation :orientation
         [l2p1 l2p2] :points} line2]
    (when (not= l1-orientation l2-orientation)
      (cond (= :vertical l1-orientation)
            (when (and (<= (:y l1p1)
                           (:y l2p1)
                           (:y l1p2))
                       (<= (:x l2p1)
                           (:x l1p1)
                           (:x l2p2)))
              {:x (:x l1p1)
               :y (:y l2p1)})

            :else
            (when (and (<= (:x l1p1)
                           (:x l2p1)
                           (:x l1p2))
                       (<= (:y l2p1)
                           (:y l1p1)
                           (:y l2p2)))
              {:x (:x l2p1)
               :y (:y l1p1)})))))

(defn break-lines-from-intersections
  [lines]
  (let [vertical-lines (filter #(= :vertical (:orientation %)) lines)
        horizontal-lines (filter #(= :horizontal (:orientation %)) lines)
        separated-lines (apply concat
                               (for [v-line vertical-lines
                                     h-line horizontal-lines
                                     :let [intersect-point (intersecting-point? v-line h-line)]
                                     :when (not (nil? intersect-point))
                                     :let [vx (:x (first (:points v-line)))
                                           vy1 (:y (first (:points v-line)))
                                           vy2 (:y (second (:points v-line)))

                                           hx1 (:x (first (:points h-line)))
                                           hx2 (:x (second (:points h-line)))
                                           hy (:y (first (:points h-line)))

                                           v-line1 (make-line
                                                    (make-point vx vy1)
                                                    (make-point vx (:y intersect-point)))
                                           v-line2 (make-line
                                                    (make-point vx (:y intersect-point))
                                                    (make-point vx vy2))
                                           h-line1 (make-line
                                                    (make-point hx1 hy)
                                                    (make-point (:x intersect-point) hy))
                                           h-line2 (make-line
                                                    (make-point (:x intersect-point) hy)
                                                    (make-point hx2 hy))]]
                                 (filter #(not= (first (:points %)) (second (:points %)))
                                         [v-line1 v-line2 h-line1 h-line2])))]
    (vec (set separated-lines))))

(defn overlapping-lines?
  [line1 line2]
  (let [{l1-orientation :orientation
         [l1p1 l1p2] :points} line1
        {l2-orientation :orientation
         [l2p1 l2p2] :points} line2]
    (and (= l1-orientation l2-orientation)
         (cond (= :vertical l1-orientation)
               (or (<= (:x l1p1) (:x l2p1) (:x l1p2))
                   (<= (:x l2p1) (:x l1p1) (:x l2p2)))

               :else
               (or (<= (:y l1p1) (:y l2p1) (:y l1p2))
                   (<= (:y l2p1) (:y l1p1) (:y l2p2)))))))

(defn largest-red-green-rect
  [points]
  ;;
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

;;
;; This sample looks like this:
;; ..............................
;; .......#000#..................
;; .......0...0..................
;; ..#0000#000#..................
;; ..0........0..................
;; #0#000000#.0..................
;; 0.0......0.0..................
;; ###000000#0#..................
;; .0.......0....................
;; .0.......0....................
;; .0.......0....................
;; .0.......0....................
;; .#0000000#....................
;; ..............................
(def hand-sample-input (parse-input "2025/day9_sample2.txt"))
(def real-input (parse-input "2025/day9_input.txt"))

(def sample-input-lines (lines-from-points sample-input))
(def real-input-lines (lines-from-points real-input))

(def part1-sample-brute (largest-rect-brute sample-input))
(def part1-real-brute (largest-rect-brute real-input))

(def part2-sample-red-green (largest-red-green-rect sample-input))
(def part2-sample2-red-green (largest-red-green-rect hand-sample-input))
;; (def part2-real-red-green (largest-red-green-rect real-input))
