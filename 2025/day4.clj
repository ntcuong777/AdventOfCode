(ns day4
  (:require [clojure.string :as str]))

(defn parse-input [inp-file]
  (vec (map #(vec (str/trim %))
            (str/split (slurp inp-file) #"\n"))))

(def directions {:UL [-1 -1]
                 :U [-1 0]
                 :UR [-1 1]
                 :DL [1 -1]
                 :D [1 0]
                 :DR [1 1]
                 :L [0 -1]
                 :R [0 1]})

(defn count-surroundings [grid row col]
  (reduce + 0
          (for [[_ [dr dc]] directions]
            (let [r (+ row dr)
                  c (+ col dc)]
              (if (and (>= r 0)
                       (< r (count grid))
                       (>= c 0)
                       (< c (count (grid 0)))
                       (= (get-in grid [r c]) \@))
                1
                0)))))

(def sample-rolls (parse-input "2025/day4_sample.txt"))
(def actual-rolls (parse-input "2025/day4_input.txt"))

(defn bfs-rolls
  ([queue rolls] (bfs-rolls queue rolls 0))
  ([queue rolls cnt]
   (let [queue-cnt (count queue)
         new-rolls (reduce (fn [cur-rolls [r c]]
                             (assoc-in cur-rolls [r c] \x))
                           rolls
                           queue)
         new-queue (set
                    (mapcat (fn [[r c]]
                              (filter #(not (nil? %))
                                      (for [[_ [dr dc]] directions]
                                        (let [nr (+ r dr)
                                              nc (+ c dc)
                                              val (get-in new-rolls [nr nc] \x)
                                              cnt (count-surroundings new-rolls nr nc)]
                                          (when (and (= val \@)
                                                     (< cnt 4))
                                            [nr nc])))))
                            queue))]
     (if (empty? new-queue)
       (+ cnt queue-cnt)
       (recur new-queue new-rolls (+ cnt queue-cnt))))))

(defn solve [rolls iterate?]
  (let [init-queue (set (filter #(not (nil? %))
                                (for [n (range (count rolls))
                                      m (range (count (rolls n)))]
                                  (when-let [cnt (count-surroundings rolls n m)]
                                    (when (and (< cnt 4)
                                               (= \@ (get-in rolls [n m])))
                                      [n m])))))]
    (if iterate? (bfs-rolls init-queue rolls) (count init-queue))))

(def sample-result-part1 (solve sample-rolls false))
(def part1-result (solve actual-rolls false))
(def sample-result-part2 (solve sample-rolls true))
(def part2-result (solve actual-rolls true))
