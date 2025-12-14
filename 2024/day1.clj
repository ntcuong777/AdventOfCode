(ns AdventOfCode.2024.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [input]
  (let [vecs (map (fn [line]
                    (map #(Integer/parseInt %)
                         (str/split line #" +")))
                  (with-open [rdr (io/reader input)]
                    (doall (line-seq rdr))))]
    [(map first vecs)
     (map second vecs)]))

(def vecs (parse-input "/Users/ntcuong777/personal-projects/AdventOfCode/2024/day1_input.txt"))

(filter nil? (first vecs))
(reduce + (map #(abs (- %1 %2))
               (sort (first vecs)) (sort (second vecs))))
