(ns day10
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;;
;; TODO:
;; Peaked at the right solution for part 2 on reddit
;; I feel stupid for not spending more time on it
;; so I would skip part 2 for now
;; maybe will come back later
;; when I forget the solution I saw
;; and attempt to solve it myself
;;
;; Not that seeing the correct solution is a bad thing
;; just that my stupid ego hurts a bit
;;

(defn parse-light-spec
  [light-spec]
  (let [cleaned-spec (str/replace light-spec #"^\[|\]$" "")]
    (reduce (fn [state [idx char]]
              (if (= char \#)
                (bit-set state idx)
                state))
            0
            (zipmap (range) cleaned-spec))))

(defn parse-button
  [button-str]
  (let [stripped (str/replace button-str #"^\(|\)$" "")
        button-nums (map #(Long/parseLong %)
                         (str/split stripped #","))
        button-toggle-states (reduce (fn [state num]
                                       (bit-set state num))
                                     0
                                     button-nums)]
    button-toggle-states))

(defn parse-data-line
  [line-data]
  (let [final-lights (first line-data)
        buttons (rest (butlast line-data))
        joltage (last line-data)]
    {:light-spec (parse-light-spec final-lights)
     :buttons (vec (map parse-button buttons))
     :joltage joltage}))

(defn parse-input
  [inp-file]
  (->> (slurp inp-file)
       str/split-lines
       (map #(str/split % #"\s+"))
       (map parse-data-line)
       vec))

(defn press-buttons
  [buttons button-state]
  (reduce (fn [state [idx button]]
            (if (bit-test button-state idx)
              (bit-xor state button)
              state))
          0
          (zipmap (range) buttons)))

(defn get-num-steps-to-get-spec
  [machine]
  (let [{:keys [light-spec buttons]} machine
        n-buttons (count buttons)
        max-states (bit-shift-left 1 n-buttons)]
    (loop [buttons-state 0
           min-buttons (inc n-buttons)]
      (if (= buttons-state max-states)
        min-buttons
        (let [pressed-lights (press-buttons buttons buttons-state)
              num-steps (Long/bitCount buttons-state)
              new-min-buttons (if (= pressed-lights light-spec)
                                (min min-buttons num-steps)
                                min-buttons)]
          (recur (inc buttons-state) new-min-buttons))))))

(def sample-input (parse-input "2025/day10_sample.txt"))
(def real-input (parse-input "2025/day10_input.txt"))

(defn part-1
  [input-data]
  (reduce + (map get-num-steps-to-get-spec input-data)))
