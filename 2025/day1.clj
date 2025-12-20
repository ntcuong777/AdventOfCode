(ns AdventOfCode.2025.day1
  (:require [clojure.string :as str]))

(defn parse-input [inp-file]
  (map (fn [x]
         (let [sign (if (= (first x) \L) -1 1)]
           (* sign (Integer/parseInt (.substring x 1)))))
       (clojure.string/split (slurp inp-file) #"\n")))

;; Part 1
(def rotations (parse-input "2025/day1_input.txt"))
(def rotations-sample (parse-input "2025/day1_sample.txt"))

(def positions1 (reductions (fn [acc x]
                              (mod (+ acc x) 100))
                            50
                            rotations))
(def part1 (count (filter #(= 0 %) positions1)))

(def part2 (reductions (fn [[seen pos] x]
                         (let [num-passing-zeros-pre-under-100 (abs (int (/ x 100)))
                               final-x-under-100 (if (< x 0)
                                                   (+ x (* num-passing-zeros-pre-under-100 100))
                                                   (- x (* num-passing-zeros-pre-under-100 100)))
                               additional-zero? (if (or (and (< final-x-under-100 0)
                                                             (> pos 0)
                                                             (<= pos (- final-x-under-100)))
                                                        (and (> final-x-under-100 0)
                                                             (>= final-x-under-100 (- 100 pos))))
                                                  1 0)]
                           [(+ seen num-passing-zeros-pre-under-100 additional-zero?)
                            (mod (+ pos final-x-under-100) 100)]))
                       [0 50]
                       rotations))

(first (last part2))
