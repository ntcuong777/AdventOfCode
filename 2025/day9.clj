(ns day9
  (:require [clojure.string :as str]))

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

(defn check-point-line-valid [[px py] [[lx1 ly1] [lx2 ly2]]]
  "If line x1 == x2, vertical line: if lx1 < px then either min-y <= py or max-y >= py")

(defn largest-red-green-rect [points]
  (let [point-set (set points)
        lines (map #(vector %1 %2)
                   points
                   (conj (vec (rest points)) (first points)))
        valid-pairs (for [p1 points
                          p2 points
                          :when (and (not= p1 p2)
                                     (let [corners (rect-corners p1 p2)]
                                       (every? (fn [p]
                                                 (or (point-set p)
                                                     (some #(on-line? % p) lines)))
                                               corners)))]
                      [p1 p2])
        valid-areas (map (fn [[p1 p2]]
                           (area p1 p2))
                         valid-pairs)]
    ;; (println "Valid pairs count:" (count valid-pairs))
    ;; (println "Valid pairs:" valid-pairs)
    (apply max valid-areas)))

(def sample-input (parse-input "2025/day9_sample.txt"))
(def real-input (parse-input "2025/day9_input.txt"))

(def part1-sample-brute (largest-rect-brute sample-input))
(def part1-real-brute (largest-rect-brute real-input))

(def part2-sample-red-green (largest-red-green-rect sample-input))
(def part2-real-red-green (largest-red-green-rect real-input))
