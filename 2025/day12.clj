(ns day12
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME: This is a simple, barely correct approximation (doesn't event mathematically correct)
;; of the actual problem for Advent Of Code 2025 Day 12.
;; The real problem should be solved using constraint satisfaction and complex shape
;; manipulation to compute the right constraints for SAT solving.
;; But as my time is limited and I mostly want to solve the problems for fun,
;; I'm providing a fake solution that just counts the number of regions
;; that could possibly fit the shapes based on area only.
;;
;; Potentially, I will work on the real solution in the future when I have more time.
;; But, well, life is short and busy for everyone and I want to prioritize focusing on feeding
;; my family over my own hobbies and interests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-region [region-line]
  (let [[size spec] (str/split region-line #":")
        [w h] (str/split size #"x")
        shape-specs (vec (map #(Long/parseLong %)
                              (str/split (str/trim spec) #" ")))]
    {:size [(Long/parseLong w) (Long/parseLong h)]
     :shapes shape-specs}))

(defn merge-shape-with-line
  [shape line]
  (let [{:keys [raw-shape hash-count]} shape
        new-raw-shape (conj raw-shape line)
        new-hash-count (+ (or hash-count 0) (count (re-seq #"#" line)))]
    {:raw-shape new-raw-shape
     :hash-count new-hash-count}))

(defn merge-lines-reducer
  [cur-state line]
  (if (empty? line)
    cur-state
    (let [line-starts-with-num? (re-find #"^\d" line)
          line-ends-with-colon? (re-find #":$" line)
          line-starts-with-dot-or-hash? (re-find #"^[.#]" line)
          is-region? (and line-starts-with-num?
                          (not line-ends-with-colon?))
          {:keys [regions shapes]} cur-state
          last-shape (last shapes)]
      (cond
        is-region? (let [region (parse-region line)]
                     (update cur-state :regions conj region))

        ;; new shape line
        (and (not is-region?)
             line-starts-with-num?)
        (update cur-state :shapes conj [])

        ;; continuing shape line
        (and (not is-region?)
             line-starts-with-dot-or-hash?)
        (let [updated-shape (merge-shape-with-line
                             last-shape
                             line)]
          (update cur-state :shapes
                  #(conj (vec (butlast %)) updated-shape)))

        :else cur-state))))

(defn parse-input [input-file]
  (->> (slurp input-file)
       str/split-lines
       (reduce merge-lines-reducer
               {:regions [] :shapes []})))

(defn fake-solve-part1
  [input]
  (let [{:keys [regions shapes]} input
        get-hash-count (fn [shape-id]
                         (get-in shapes [shape-id :hash-count]))]
    (count
     (filter (fn [reg]
               (let [[w h] (:size reg)
                     area (* w h)
                     shape-specs (:shapes reg)
                     min-space (apply + (map-indexed
                                         (fn [idx spec]
                                           (* spec (get-hash-count idx)))
                                         shape-specs))]
                 ;; (println "Region size:" w "x" h "Area:" area "Min space needed:" min-space)
                 (>= area min-space)))
             regions))))

(def sample-input (parse-input "2025/day12_sample.txt"))
(def actual-input (parse-input "2025/day12_input.txt"))
