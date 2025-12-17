(ns day5
  (:require [clojure.string :as str])
  (:import (java.util Comparator)))

(defn parse-input [inp-file]
  (->> (slurp inp-file)
       (str/split-lines)
       (reduce (fn [{:keys [ranges ingredients]} line]
                 (cond
                   (= 0 (count (str/trim line)))
                   {:ranges ranges :ingredients []}

                   (not (nil? ingredients))
                   {:ranges ranges
                    :ingredients (conj ingredients (Integer/parseInt (str/trim line)))}

                   :else
                   {:ranges (conj ranges (map #(Integer/parseInt (str/trim %))
                                              (str/split line #"-")))
                    :ingredients ingredients}))
               {:ranges [] :ingredients nil})))

(defn preprocess-input [data]
  (assoc data :ranges (sort (reify Comparator
                              (compare [_ a b]
                                (let [first-res (compare (first a) (first b))
                                      second-res (compare (second a) (second b))]
                                  (if (not= 0 first-res)
                                    first-res
                                    second-res)))

                              (equals [this b]
                                (and (= (first this) (first b))
                                     (= (second this) (second b)))))
                            (:ranges data))))

(def sample-data (-> (parse-input "2025/day5_sample.txt")
                     preprocess-input))
