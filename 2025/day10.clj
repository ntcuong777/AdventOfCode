(ns day10
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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

(defn parse-joltage
  [joltage-str]
  (let [stripped (str/replace joltage-str #"^\{|\}$" "")]
    (vec (map #(Long/parseLong %)
              (str/split stripped #",")))))

(defn parse-data-line
  [line-data]
  (let [final-lights (first line-data)
        buttons (rest (butlast line-data))
        joltage (last line-data)]
    {:light-spec (parse-light-spec final-lights)
     :buttons (vec (map parse-button buttons))
     :joltage (parse-joltage joltage)}))

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

;;
;; Part 2:
;; The answer to each machine's smallest button presses to reach the spec. joltage config
;; must look like:
;;
;; x1*button1 + x2*button2 + ... + xn*buttonn = joltage
;;
;; assuming button1, button2, ..., buttonn and joltage are vectors
;; and button1, ... are vectors of 0s and 1s.
;; x1, x2, ..., xn must contain some even and some odd integers.
;; -> We can first eliminate the odd integers by pressing each button
;; of odd xi once, then we can halve the remaining even integers and solve
;; the same problem again.
;;

(defn all-even
  [nums]
  (every? even? nums))

(defn press-button-on-joltage
  [joltage button]
  (vec (map-indexed (fn [idx jolt]
                      (if (bit-test button idx)
                        (dec jolt)
                        jolt))
                    joltage)))

(defn check-valid-presses-for-joltage
  [button-state buttons joltage]
  (let [final-joltage (reduce (fn [cur-joltage [idx button]]
                                (if (bit-test button-state idx)
                                  (press-button-on-joltage cur-joltage button)
                                  cur-joltage))
                              joltage
                              (zipmap (range) buttons))]
    (if (and (all-even final-joltage)
             (every? #(>= % 0) final-joltage))
      [true final-joltage]
      [false nil])))

(defn get-pressed-buttons
  [button-state buttons]
  (reduce (fn [pressed [idx button]]
            (if (bit-test button-state idx)
              (conj pressed button)
              pressed))
          []
          (zipmap (range) buttons)))

(defn find-buttons-for-odd-joltages
  [buttons joltage]
  (let [num-buttons (count buttons)]
    (loop [button-state 0
           valid-joltage-configs {}]
      (if (= button-state (bit-shift-left 1 num-buttons))
        valid-joltage-configs
        (let [[is-valid-presses? final-joltage] (check-valid-presses-for-joltage
                                                 button-state
                                                 buttons
                                                 joltage)
              num-presses (Long/bitCount button-state)]
          (if is-valid-presses?
            (let [new-joltage-configs (assoc valid-joltage-configs
                                             button-state
                                             {:num-presses num-presses
                                              :joltage final-joltage})]
              (recur (inc button-state)
                     new-joltage-configs))
            (recur (inc button-state)
                   valid-joltage-configs)))))))

(defn halve-joltage
  [joltage]
  (vec (map #(quot % 2) joltage)))

(def infinite Long/MAX_VALUE)

(defn count-joltage-steps
  [{:keys [buttons joltage]}]
  (if (all-even joltage)
    (if (every? #(= % 0) joltage)
      0
      (let [count (count-joltage-steps {:buttons buttons
                                        :joltage (halve-joltage joltage)})]
        (if (= count infinite)
          infinite
          (* 2 count))))
    (let [new-joltage-configs (find-buttons-for-odd-joltages
                               buttons
                               joltage)]
      ;; (println "new configs:" new-joltage-configs)
      (println "num configs:" (count new-joltage-configs) "new configs:" new-joltage-configs "for joltage:" joltage "and buttons" buttons)
      (if (empty? new-joltage-configs)
        infinite
        (apply min (map (fn [[_button-state {:keys [num-presses joltage]}]]
                          (let [count-config (count-joltage-steps {:buttons buttons
                                                                   :joltage joltage})]
                            (if (= count-config infinite)
                              infinite
                              (+ num-presses count-config))))
                        new-joltage-configs))))))

(def sample-input (parse-input "2025/day10_sample.txt"))
(def real-input (parse-input "2025/day10_input.txt"))

(defn part-1
  [input-data]
  (reduce + (map get-num-steps-to-get-spec input-data)))

(defn part-2
  [input-data]
  (reduce + (map count-joltage-steps input-data)))
