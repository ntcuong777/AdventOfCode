(ns day5
  (:require [clojure.string :as str])
  (:import [java.util Collections]))

(defn parse-input [inp-file]
  (->> (slurp inp-file)
       (str/split-lines)
       (reduce (fn [{:keys [ranges ingredients]} line]
                 (cond
                   (= 0 (count (str/trim line)))
                   {:ranges ranges :ingredients []}

                   (not (nil? ingredients))
                   {:ranges ranges
                    :ingredients (conj ingredients (Long/parseLong (str/trim line)))}

                   :else
                   {:ranges (conj ranges (map #(Long/parseLong (str/trim %))
                                              (str/split line #"-")))
                    :ingredients ingredients}))
               {:ranges [] :ingredients nil})))

(defn preprocess-input [data]
  (let [data-w-sorted-ranges (assoc data :ranges (vec (map vec (sort (fn [a b]
                                                                       (compare (vec a) (vec b)))
                                                                     (:ranges data)))))
        final-data (assoc data-w-sorted-ranges :ranges-upper-prefix-max
                          (vec (reduce (fn [acc curr]
                                         (conj acc (max (last curr)
                                                        (if (empty? acc)
                                                          Long/MIN_VALUE
                                                          (last acc)))))
                                       []
                                       (:ranges data-w-sorted-ranges))))]
    final-data))

(defn is-fresh [data ingr]
  (let [ranges (:ranges data)
        lower-bound-ranges (vec (map first ranges))
        upper-bound-prefix (:ranges-upper-prefix-max data)
        first-larger-idx (let [found-idx (Collections/binarySearch lower-bound-ranges (+ ingr 1))]
                           ;; (println "Binary search for" (+ ingr 1) "in" lower-bound-ranges "found idx:" found-idx)
                           (if (neg? found-idx)
                             (- (- found-idx) 1)
                             found-idx))
        largest-upper-bound-til-first-larger (if (> first-larger-idx 0)
                                               (nth upper-bound-prefix (- first-larger-idx 1))
                                               Long/MIN_VALUE)]
    ;; (println "Ranges:" ranges " Lower bounds:" lower-bound-ranges " Upper bound prefix:" upper-bound-prefix)
    ;; (println "Ingredient:" ingr " First larger idx:" first-larger-idx " Largest upper bound till first larger:" largest-upper-bound-til-first-larger)
    (>= largest-upper-bound-til-first-larger ingr)))

(defn merge-ranges [sorted-ranges]
  (let [merged (reduce (fn [acc curr]
                         (if (empty? acc)
                           [curr]
                           (let [last-range (last acc)]
                             (if (<= (first curr) (inc (last last-range)))
                               (conj (vec (butlast acc))
                                     [(first last-range)
                                      (max (last last-range) (last curr))])
                               (conj acc curr)))))
                       []
                       sorted-ranges)
        count-nums-in-ranges (reduce (fn [acc curr]
                                       (+ acc
                                          (- (inc (last curr))
                                             (first curr))))
                                     0
                                     merged)]
    count-nums-in-ranges))

(def sample-data (-> (parse-input "2025/day5_sample.txt")
                     preprocess-input))
(def count-sample-fresh (count (filter #(is-fresh sample-data %) (:ingredients sample-data))))

(def real-data (-> (parse-input "2025/day5_input.txt")
                   preprocess-input))
(def count-part1-fresh (count (filter #(is-fresh real-data %) (:ingredients real-data))))

(def sample-merged-ranges-count (merge-ranges (:ranges sample-data)))
(def part2-merged-ranges-count (merge-ranges (:ranges real-data)))
