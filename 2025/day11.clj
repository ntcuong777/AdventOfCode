(ns day11
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-graph
  [line]
  (let [[node neighbors-str] (str/split line #": ")
        neighbors (str/split neighbors-str #" ")]
    {node (set neighbors)}))

(defn parse-input [inp-file]
  (->> (slurp inp-file)
       str/split-lines
       (map parse-graph)
       (apply merge)))

(defn reverse-graph
  [graph]
  (reduce (fn [acc [node neighbors]]
            (reduce (fn [a neighbor]
                      (update a neighbor (fnil conj #{}) node))
                    acc
                    neighbors))
          {}
          graph))

(defn bfs
  [graph start]
  (loop [queue [start]
         ;; visited is a map of node to distance from start
         visited {start 0}]
    (if (empty? queue)
      visited
      (let [current (first queue)
            current-distance (get visited current)
            neighbors (get graph current #{})
            visited-nodes (set (keys visited))
            new-neighbors (set/difference neighbors visited-nodes)
            new-visited (reduce (fn [v n]
                                  (assoc v n (inc current-distance)))
                                visited
                                new-neighbors)
            new-queue (concat (rest queue) new-neighbors)]
        (recur new-queue new-visited)))))

(defn count-in-out-degrees
  [graph]
  (reduce (fn [acc [node neighbors]]
            (let [acc (update-in acc [node :out] (fnil + 0) (count neighbors))
                  acc (reduce (fn [a neighbor]
                                (update-in a [neighbor :in] (fnil inc 0)))
                              acc
                              neighbors)]
              acc))
          {}
          graph))

(defn part-1-count-paths
  [input-graph]
  (let [reversed-graph (reverse-graph input-graph)
        reachable-out (bfs reversed-graph "out")
        reachable-you (bfs input-graph "you")
        reachable-both (set/intersection (set (keys reachable-out))
                                         (set (keys reachable-you)))
        ;; Build reduced graph with only nodes reachable from both "you" and "out"
        reduced-graph (reduce (fn [acc [node neighbors]]
                                (if (contains? reachable-both node)
                                  (assoc acc node (set/intersection neighbors reachable-both))
                                  acc))
                              {}
                              input-graph)
        reduced-graph-degrees (count-in-out-degrees reduced-graph)]
    (loop [queue ["you"]
           degrees reduced-graph-degrees
           path-counts {"you" 1}]
      (if (empty? queue)
        (get path-counts "out" 0)
        (let [current (first queue)
              current-path-count (get path-counts current 0)
              neighbors (get reduced-graph current #{})
              [new-degrees new-path-counts new-queue]
              (reduce (fn [[deg pc q] neighbor]
                        (let [updated-pc (update pc neighbor (fnil + 0) current-path-count)
                              updated-deg (update-in deg [neighbor :in] dec)]
                          (if (= 0 (get-in updated-deg [neighbor :in]))
                            [updated-deg updated-pc (conj q neighbor)]
                            [updated-deg updated-pc q])))
                      [degrees path-counts (rest queue)]
                      neighbors)]
          (recur new-queue new-degrees new-path-counts))))))

(def sample-input (parse-input "2025/day11_sample.txt"))
(def real-input (parse-input "2025/day11_input.txt"))
