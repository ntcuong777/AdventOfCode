(ns day6
  (:require [clojure.string :as str]))

(defn parse-input [inp-file]
  (->> (slurp inp-file)
       (str/split-lines)
       (map (fn [line]
              (let [splits (str/split (str/trim line) #" ")]
                {:parsed
                 (->> splits
                      (map (fn [s]
                             (let [trimmed (str/trim s)
                                   num? (re-matches #"\d+" trimmed)]
                               (if (not (nil? num?))
                                 (Long/parseLong num?)
                                 ;; operator, either + or *
                                 trimmed))))
                      (filter #(not (= % ""))))

                 :unparsed splits
                 :original line})))))

(defn grand-total [data & {:keys [parsed-key] :or {parsed-key :parsed}}]
  (let [n-probs (count (get (first data) parsed-key))
        n-operands (dec (count data))
        operators (get (last data) parsed-key)]
    (reduce + 0
            (for [prob-idx (range n-probs)
                  :let [operator (nth operators prob-idx)
                        operands (map #(nth (get (nth data %) parsed-key) prob-idx)
                                      (range n-operands))]]
              (if (= operator "+")
                (apply + operands)
                (apply * operands))))))

(defn grand-total-part2 [data]
  (let [n-operands (dec (count data))
        max-line-len (apply max (map #(count (:original %)) data))
        raw-operands (map (fn [idx]
                            (let [operand (map #(get-in (:original (nth data %))
                                                        [idx]
                                                        \space)
                                               (range n-operands))]
                              (str/trim (apply str operand))))
                          (range max-line-len))
        parsed-operands (reduce (fn [acc op-str]
                                  (if (= op-str "")
                                    (conj acc [])
                                    (let [last-ops (last acc)
                                          new-last-ops (conj last-ops
                                                             (Long/parseLong op-str))]
                                      (conj (vec (butlast acc))
                                            new-last-ops))))
                                [[]]
                                raw-operands)
        per-prob-result (map (fn [operator operands]
                               (if (= operator "+")
                                 (apply + operands)
                                 (apply * operands)))
                             (:parsed (last data))
                             parsed-operands)
        final-total (reduce + 0 per-prob-result)]
    final-total))

(def sample-data (parse-input "2025/day6_sample.txt"))
(def real-data (parse-input "2025/day6_input.txt"))

(def grand-total-sample (grand-total sample-data :parsed-key :parsed))
(def grand-total-part1 (grand-total real-data :parsed-key :parsed))

(def grand-total-sample-part2 (grand-total-part2 sample-data))
(def grand-total-part2 (grand-total-part2 real-data))
