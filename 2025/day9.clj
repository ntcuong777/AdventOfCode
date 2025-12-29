(ns day9
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input
  [inp-file]
  (->> (slurp inp-file)
       str/split-lines
       (map #(let [[x y] (vec (str/split % #","))]
               [(Long/parseLong x) (Long/parseLong y)]))))

(defn make-point
  [x y]
  {:x x
   :y y})

(defn points-as-map [vec-points]
  (vec (map (fn [[x y]]
              (make-point x y))
            vec-points)))

(defn area
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  (let [width (inc (Math/abs (- x2 x1)))
        height (inc (Math/abs (- y2 y1)))]
    (* width height)))

(defn largest-rect-brute
  [raw-points]
  (let [points-map (points-as-map raw-points)
        point-pairs (for [p1 points-map
                          p2 points-map
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
  (map make-line
       points
       (conj (vec (rest points)) (first points))))

(defn line-ymin [line]
  (let [{[{y1 :y} {y2 :y}] :points} line]
    (min y1 y2)))

(defn line-ymax [line]
  (let [{[{y1 :y} {y2 :y}] :points} line]
    (max y1 y2)))

(defn line-xmin [line]
  (let [{[{x1 :x} {x2 :x}] :points} line]
    (min x1 x2)))

(defn line-xmax [line]
  (let [{[{x1 :x} {x2 :x}] :points} line]
    (max x1 x2)))

(defn point-on-lines?
  [point lines]
  (let [{px :x
         py :y} point]
    (some (fn [{orientation :orientation
                points :points}]
            (case orientation
              :vertical (let [line-x (:x (first points))
                              min-y (:y (first points))
                              max-y (:y (second points))]
                          (and (= line-x px)
                               (<= min-y py max-y)))
              :horizontal (let [line-y (:y (first points))
                                min-x (:x (first points))
                                max-x (:x (second points))]
                            (and (= line-y py)
                                 (<= min-x px max-x)))))
          lines)))

(defn is-vert-lines-on-same-side-of-y?
  "only works for two vertical lines having an end at y"
  [v1 v2 y]
  (let [v1-ymin (line-ymin v1)
        v1-ymax (line-ymax v1)
        v2-ymin (line-ymin v2)
        v2-ymax (line-ymax v2)]
    (when (or (and (not= v1-ymin y)
                   (not= v1-ymax y))
              (and (not= v2-ymin y)
                   (not= v2-ymax y)))
      (throw (Exception. "Lines do not have an end at y")))

    (cond
      (or (and (= v1-ymin y)
               (= v2-ymin y))
          (and (= v1-ymax y)
               (= v2-ymax y))) true

      :else false)))

(defn in-out-finder-reducer
  [x-to-vert-line]
  (let [toggle-state (fn [state]
                       (case state
                         :inside :outside
                         :outside :inside))]
    (fn [{cur-state :state
          on-horiz-line? :on-horiz-line? ;; nil if not on horiz line, else a map of {:pre-line-state ..., :line ...}
          remain-horiz :horiz ;; assuming sorted by x
          :as acc}
         x]
      (cond (nil? on-horiz-line?)
            (let [fst-line (if (seq remain-horiz)
                             (first remain-horiz)
                             (make-line
                              (make-point Long/MAX_VALUE Long/MAX_VALUE)
                              (make-point Long/MAX_VALUE Long/MAX_VALUE))) ;; dummy horiz line far right
                  fx1 (line-xmin fst-line)
                  fx2 (line-xmax fst-line)
                  vert-line-at-x (get x-to-vert-line x)]
              (if (and (<= fx1 x) (< x fx2))
               ;; on the horiz line -> inside
                {:state :inside
                 :on-horiz-line? {:line (first remain-horiz) :pre-line-state cur-state}
                 :horiz (rest remain-horiz)}
                {:state (if (not (nil? vert-line-at-x))
                          (toggle-state cur-state) ;; passed a vert line
                          cur-state)
                 :on-horiz-line? nil
                 :horiz remain-horiz}))

            :else
            (let [{cur-horiz-line :line
                   pre-line-state :pre-line-state} on-horiz-line?
                  vert-line-at-x (get x-to-vert-line x)
                  hx1 (line-xmin cur-horiz-line)
                  hx2 (line-xmax cur-horiz-line)
                  hy (line-ymin cur-horiz-line)
                  vert-line-at-hx1 (get x-to-vert-line hx1)]
              (cond (and (< hx1 x) (< x hx2))
                    acc

                    (= x hx2)
                    {:state (if (not (is-vert-lines-on-same-side-of-y?
                                      vert-line-at-hx1
                                      vert-line-at-x
                                      hy))
                              ;; if the vert line at the earlier end has different side (one is up and one is down, for ex)
                              ;; then, when we exit the horiz line, we need to toggle the state "before" we enter the horiz line
                              (toggle-state pre-line-state)
                              pre-line-state)
                     :on-horiz-line? nil
                     :horiz remain-horiz}))))))

(defn is-xs-on-same-y-in-shape?
  "Assuming horizontal lines are sorted by x"
  [xs horiz-lines-at-y x-to-vert-line]
  (let [;; alternating between inside and outside as we pass each line
        ;; an intuition is that whenever we encounter a vertical line at x < px
        ;; then we are inside if we were outside before, and vice versa
        tmp-x-in-shape-result (reductions (in-out-finder-reducer x-to-vert-line)
                                          {:state :outside
                                           :on-horiz-line? nil
                                           :horiz horiz-lines-at-y}
                                          xs)
        x-in-shape-map (apply merge
                              (map (fn [{state :state} x]
                                     {x (= state :inside)})
                                   (rest tmp-x-in-shape-result) ;; skip initial acc
                                   xs))]
    x-in-shape-map))

(defn all-rect-edges-points
  [rp1 rp2 all-anchor-xs all-anchor-ys]
  (let [{x1 :x y1 :y} rp1
        {x2 :x y2 :y} rp2
        min-x (min x1 x2)
        max-x (max x1 x2)
        min-y (min y1 y2)
        max-y (max y1 y2)]
    (for [ax all-anchor-xs
          ay all-anchor-ys
          :when (and (<= min-x ax max-x)
                     (<= min-y ay max-y)
                     (or (= ax min-x)
                         (= ax max-x)
                         (= ay min-y)
                         (= ay max-y)))]
      (make-point ax ay))))

(defn points-in-shape-reducer
  "assume vertical lines are sorted by ymin,ymax
   while horizontal lines are sorted by xmin,xmax"
  [horiz-lines xs]
  (let [y-to-horiz-lines (group-by #(get-in % [:points 0 :y])
                                   horiz-lines)]
    (fn [{points-in-shape :points-in-shape
          cur-x-to-vert-line :x-to-vert-lines
          cur-vert-lines :vert-lines}
         cur-y]
      ;; (println "Processing y =" cur-y "with" (count cur-vert-lines) "remaining vert lines")
      ;; (println "current x to vert lines:" cur-x-to-vert-line)
      ;; (println "current points-in-shape:" points-in-shape)
      (let [new-vert-lines (take-while #(<= (line-ymin %) cur-y) cur-vert-lines)
            remaining-vert-lines (drop-while #(<= (line-ymin %) cur-y) cur-vert-lines)
            x-to-new-vert-line (apply merge
                                      (map (fn [{points :points :as line}]
                                             {(get-in points [0 :x]) line})
                                           new-vert-lines))
            new-x-to-vert-line (->> (merge cur-x-to-vert-line x-to-new-vert-line)
                                    (filter (fn [[_ line]]
                                              (>= (line-ymax line) cur-y)))
                                    (map (fn [[x line]] {x line}))
                                    (apply merge))
            horiz-lines-at-y (get y-to-horiz-lines cur-y [])
            x-in-shape-map (is-xs-on-same-y-in-shape? xs
                                                      horiz-lines-at-y
                                                      new-x-to-vert-line)]
        ;; (println "at y =" cur-y " found xs-in-shape:" x-in-shape-map)
        {:points-in-shape (concat points-in-shape
                                  (for [x xs
                                        :when (get x-in-shape-map x)]
                                    (make-point x cur-y)))
         :x-to-vert-lines new-x-to-vert-line
         :vert-lines remaining-vert-lines}))))

(defn largest-rect-inside-shape
  [raw-points]
  (let [points-map (points-as-map raw-points)
        lines (lines-from-points points-map)
        ;; for every x (or y), add points preceding and succeeding each coord
        ;; so the check for whether a rect is inside the shape is more robust
        all-anchor-xs  (vec (sort (set (mapcat (fn [{[{px1 :x} {px2 :x}] :points}]
                                                 [px1 (inc px1)
                                                  px2 (inc px2)])
                                               lines))))
        all-anchor-ys (vec (sort (set (mapcat (fn [{[{py1 :y} {py2 :y}] :points}]
                                                [py1 (inc py1)
                                                 py2 (inc py2)])
                                              lines))))

        ;; vertical lines sorted by ymin,ymax
        vert-lines (sort-by (fn [line]
                              [(line-ymin line)
                               (line-ymax line)])
                            (filter #(= (:orientation %) :vertical) lines))
        ;; horizontal lines sorted by xmin,xmax
        horiz-lines (sort-by (fn [line]
                               [(line-xmin line)
                                (line-xmax line)])
                             (filter #(= (:orientation %) :horizontal) lines))

        points-in-shape-result (reduce (points-in-shape-reducer horiz-lines
                                                                all-anchor-xs)
                                       {:points-in-shape []
                                        :x-to-vert-lines {}
                                        :vert-lines vert-lines}
                                       all-anchor-ys)
        points-in-shape-set (set (:points-in-shape points-in-shape-result))
        valid-rect-areas (for [rp1 points-map
                               rp2 points-map
                               :when (not= rp1 rp2)
                               :let [rect-points (all-rect-edges-points rp1
                                                                        rp2
                                                                        all-anchor-xs
                                                                        all-anchor-ys)
                                     all-points-in-shape? (every? points-in-shape-set rect-points)]
                               ;; :when all-points-in-shape?
                               ]
                           (do
                             ;; (println "Checking rect:" rp1 rp2 "with rect points count:" (count rect-points) "all in shape?" all-points-in-shape?)
                             ;; (println "rect points:" rect-points)
                             (if all-points-in-shape?
                               (area rp1 rp2)
                               0)))]
    ;; (println "[pre-return] logging intermediates:")
    ;; (println "all-anchor-xs:" all-anchor-xs)
    ;; (println "all-anchor-ys:" all-anchor-ys)
    ;; (println "max-lines-per-x" (apply max (map count (vals x-to-vert-lines))))
    ;; (println "max-lines-per-y" (apply max (map count (vals y-to-horiz-lines))))
    ;; (println "x-to-vert-lines:" x-to-vert-lines)
    ;; (println "y-to-horiz-lines:" y-to-horiz-lines)
    ;; (println "points-in-shape count:" (count points-in-shape))
    ;; (println "points-in-shape sample:" (take 10 (sort-by #(vector (:y %) (:x %)) points-in-shape)))
    (println "final max:" (apply max valid-rect-areas))
    (apply max valid-rect-areas)))

(def sample-input (parse-input "2025/day9_sample.txt"))
(def real-input (parse-input "2025/day9_input.txt"))
(def sample-input2 (parse-input "2025/day9_sample2.txt"))

(def part1-sample-brute (largest-rect-brute sample-input))
(def part1-real-brute (largest-rect-brute real-input))

(def part2-sample-red-green (largest-rect-inside-shape sample-input))
(def part2-sample2-red-green (largest-rect-inside-shape sample-input2))
(def part2-real-red-green (largest-rect-inside-shape real-input))
