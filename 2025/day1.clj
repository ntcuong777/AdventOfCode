(ns AdventOfCode.2025.day1
  (:require [clojure.string :as str]))

(defn parse-input [inp-file]
  (map (fn [x]
         (let [sign (if (= (first x) \L) -1 1)]
           (* sign (Integer/parseInt (.substring x 1)))))
       (clojure.string/split (slurp inp-file) #"\n")))

;; Part 1
(def rotations (parse-input "/Users/ntcuong777/personal-projects/AdventOfCode/2025/day1_input.txt"))
(def rotations-sample (parse-input "/Users/ntcuong777/personal-projects/AdventOfCode/2025/day1_sample.txt"))

(def positions1 (reductions (fn [acc x]
                              (mod (+ acc x 100) 100)) 50 rotations))
(def part1 (count (filter #(= 0 %) positions1)))

(def part2 (reductions (fn [[seen pos] x]
                         (let [zero-rotations (abs (int (/ x 100)))
                               reduced-x (if (< x 0)
                                           (+ x (* zero-rotations 100))
                                           (- x (* zero-rotations 100)))
                               additional-zeros (if (or (and (< reduced-x 0)
                                                             (> pos 0)
                                                             (<= pos (- reduced-x)))
                                                        (and (> reduced-x 0)
                                                             (>= reduced-x (- 100 pos))))
                                                  1 0)]
                           [(+ seen zero-rotations additional-zeros)
                            (mod (+ pos reduced-x) 100)]))
                       [0 50]
                       rotations))

(first (last part2))
