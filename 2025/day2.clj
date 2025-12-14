(ns day2
  (:require [clojure.string :as str]))

(defn parse-input [inp-file]
  (doall
   (map (fn [s]
          (let [[l r] (str/split (str/trim s) #"-")]
            [(bigint l) (bigint r)]))
        (str/split (slurp inp-file) #","))))

(defn repeat-nums-of-length
  [n times]
  (if (> (mod n times) 0)
    []
    (let [n2 (/ n times)
          pow-n2 (bigint (Math/pow 10 n2))
          start (bigint (Math/pow 10 (dec n2)))
          end (dec (bigint (Math/pow 10 n2)))]
      (map (fn [x]
             (reduce (fn [acc _]
                       (+ (* acc pow-n2) x))
                     0
                     (range times))) (range start (inc end))))))

(def ranges-sample (parse-input "2025/day2_sample.txt"))

(def repeat-nums-part1 (mapcat #(repeat-nums-of-length % 2) (range 2 11)))

(def result-sample
  (reduce (fn [acc [l r]]
            (+ acc (reduce + 0 (filter #(and (>= % l) (<= % r)) repeat-nums-part1))))
          0
          ranges-sample))

(def ranges (parse-input "2025/day2_input.txt"))

(def result-part1
  (reduce (fn [acc [l r]]
            (+ acc (reduce + 0 (filter #(and (>= % l) (<= % r)) repeat-nums-part1))))
          0
          ranges))

(def repeat-nums-part2 (mapcat (fn [n]
                                 (vec (set (mapcat #(repeat-nums-of-length n %)
                                                   (range 2 (inc n))))))
                               (range 2 11)))

(def result-sample-part2
  (reduce (fn [acc [l r]]
            (+ acc (reduce + 0 (filter #(and (>= % l) (<= % r)) repeat-nums-part2))))
          0
          ranges-sample))

(def result-part2
  (reduce (fn [acc [l r]]
            (+ acc (reduce + 0 (filter #(and (>= % l) (<= % r)) repeat-nums-part2))))
          0
          ranges))
