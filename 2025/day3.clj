(ns day3
  (:require [clojure.string :as str]))

(defn parse-input [inp-file]
  (map (fn [line]
         (vec (map #(- (int %) (int \0)) (str/trim line))))
       (str/split (slurp inp-file) #"\n")))

(def sample-banks (parse-input "2025/day3_sample.txt"))
(def puzzle-banks (parse-input "2025/day3_input.txt"))

(defn nilable-max [& args]
  (reduce (fn [a b]
            (cond
              (and (nil? a) (nil? b)) nil
              (nil? a) b
              (nil? b) a
              :else (max a b)))
          nil
          args))

(defn num-from-digits [digits]
  (reduce (fn [acc d]
            (+ (* acc 10) d))
          0
          digits))

(defn spy [v]
  (println v)
  v)

(defn exchange-and-get-max [new-digit digits]
  (let [cur-num (num-from-digits digits)]
    (reduce (fn [[mx max-digits] i]
              (let [constructed-digits (vec (concat [new-digit]
                                                    (subvec digits 0 i)
                                                    (subvec digits (+ i 1))))
                    num (num-from-digits constructed-digits)]
                (if (> num mx)
                  [num constructed-digits]
                  [mx max-digits])))
            [cur-num digits]
            (range (count digits)))))

(defn line-max [line num-digits]
  (reduce (fn [[mx collected-digits] c]
            (if (< (count collected-digits) num-digits)
              [mx (vec (cons c collected-digits))]
              (let [[new-mx new-digits] (exchange-and-get-max c collected-digits)]
                [new-mx new-digits])))
          [0 []]
          (reverse line)))

(def sample-result-part1 (reduce + 0 (map #(first (line-max % 2)) sample-banks)))
(def puzzle-result-part1 (reduce + 0 (map #(first (line-max % 2)) puzzle-banks)))

(def sample-result-part2 (reduce + 0 (map #(first (line-max % 12)) sample-banks)))
(def puzzle-result-part2 (reduce + 0 (map #(first (line-max % 12)) puzzle-banks)))
