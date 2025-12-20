(ns day7
  (:require [clojure.string :as str]))

(defn parse-input [inp-file]
  (->> (slurp inp-file)
       (str/split-lines)
       (filter #(not (= (count (str/trim %)) 0)))))

(defn count-splits-and-timelines [lines]
  (reduce (fn [[cnt-splits prev-timelines prev-line] cur-line]
            (if (nil? prev-line)
              [cnt-splits (vec (map #(if (= % \S) 1 0) cur-line)) cur-line]
              (let [make-beams-and-timelines (map-indexed (fn [idx c]
                                                            (let [prev-char (nth prev-line idx)
                                                                  prev-char--1 (get prev-line (dec idx) \.)
                                                                  prev-char-+1 (get prev-line (inc idx) \.)
                                                                  prev-timeline (get prev-timelines idx 0)
                                                                  prev-timeline--1 (get prev-timelines (dec idx) 0)
                                                                  prev-timeline-+1 (get prev-timelines (inc idx) 0)
                                                                  cur-line-+1-char (get cur-line (inc idx) \.)
                                                                  cur-line--1-char (get cur-line (dec idx) \.)]
                                                              (cond
                                                                ;; new splitter, no beam
                                                                (= c \^) [\^ 0]

                                                                ;; beams split due to surrounding splitters
                                                                (and (or (= c \.)
                                                                         (= c \|))
                                                                     (= cur-line-+1-char \^)
                                                                     (= prev-char-+1 \|)
                                                                     (= cur-line--1-char \^)
                                                                     (= prev-char--1 \|))
                                                                [\| (+ prev-timeline prev-timeline-+1 prev-timeline--1)]

                                                                ;; beam goes left due to right splitter
                                                                (and (or (= c \.)
                                                                         (= c \|))
                                                                     (= cur-line-+1-char \^)
                                                                     (= prev-char-+1 \|))
                                                                [\| (+ prev-timeline prev-timeline-+1)]

                                                                ;; beam goes right due to left splitter
                                                                (and (or (= c \.)
                                                                         (= c \|))
                                                                     (= cur-line--1-char \^)
                                                                     (= prev-char--1 \|))
                                                                [\| (+ prev-timeline prev-timeline--1)]

                                                                ;; beam continues
                                                                (and (= c \.)
                                                                     (or (= prev-char \S)
                                                                         (= prev-char \|)))
                                                                [\| prev-timeline]

                                                                ;; default, either has beam or not
                                                                :else [c prev-timeline])))
                                                          cur-line)
                    make-beams (apply str (map first make-beams-and-timelines))
                    make-timelines (vec (map second make-beams-and-timelines))
                    cnt-additional-splits (apply + (map #(if (and (= %1 \|)
                                                                  (= %2 \^))
                                                           1
                                                           0)
                                                        prev-line cur-line))]
                [(+ cnt-splits cnt-additional-splits) make-timelines make-beams])))
          [0 nil nil]
          (map str lines)))

(def sample-input (parse-input "2025/day7_sample.txt"))
(def real-input (parse-input "2025/day7_input.txt"))

(def part1-sample-count (first (count-splits-and-timelines sample-input)))
(def part1-real-count (first (count-splits-and-timelines real-input)))

(def part2-sample-timelines (apply + (second (count-splits-and-timelines sample-input))))
(def part2-real-timelines (apply + (second (count-splits-and-timelines real-input))))
