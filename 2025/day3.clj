(ns day3
  (:require [clojure.string :as str]))

(defn parse-input [inp-file]
  (map #(str/trim %) (str/split (slurp inp-file) #"\n")))

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

(defn num-from-2-char [c1 c2]
  (if (or (nil? c1) (nil? c2))
    (- (int (or c1 c2 \0)) (int \0))
    (+ (* 10 (- (int c1) (int \0)))
       (- (int c2) (int \0)))))

(defn char-max [c1 c2]
  (if (or (nil? c1) (nil? c2))
    (or c1 c2)
    (if (> (int c1) (int c2)) c1 c2)))

(defn spy [v]
  (println v)
  v)

(defn part1-line-max [line]
  (reduce (fn [[mx max-c] c]
            (println [[mx max-c] c])
            (let [new-mx (nilable-max mx
                                      (num-from-2-char c max-c))]
              [new-mx (char-max c max-c)]))
          [0 nil]
          (reverse line)))

(def sample-result (reduce + 0 (map #(first (part1-line-max %)) sample-banks)))
(def puzzle-result (reduce + 0 (map #(first (part1-line-max %)) puzzle-banks)))
