(ns day6
  (:require [clojure.string :as str]))

(defn parse-input [inp-file]
  (->> (slurp inp-file)
       (str/split-lines)
       (map (fn [line]
              (let [splits (str/split (str/trim line) #" ")]
                (->> splits
                     (map (fn [s]
                            (let [trimmed (str/trim s)
                                  num? (re-matches #"\d+" trimmed)]
                              (if (not (nil? num?))
                                (Long/parseLong num?)
                                ;; operator, either + or *
                                trimmed))))
                     (filter #(not (= % "")))))))))

(defn grand-total [data]
  (let [n-probs (count (first data))
        n-operands (dec (count data))
        operators (last data)]
    (reduce + 0
            (for [prob-idx (range n-probs)
                  :let [operator (nth operators prob-idx)
                        operands (map #(nth (nth data %) prob-idx) (range n-operands))]]
              (if (= operator "+")
                (apply + operands)
                (apply * operands))))))

(def sample-data (parse-input "2025/day6_sample.txt"))
(def real-data (parse-input "2025/day6_input.txt"))

(def grand-total-sample (grand-total sample-data))
(def grand-total-part1 (grand-total real-data))
