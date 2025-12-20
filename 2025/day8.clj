(ns day8
  (:require [clojure.string :as str]))

(defn parse-input [inp-file]
  (vec (->> (slurp inp-file)
            str/split-lines
            (map (fn [line]
                   (vec (map #(Long/parseLong %)
                             (str/split line #","))))))))

(defn euclidean-distance [point1 point2]
  (Math/sqrt (reduce + 0 (map (fn [a b] (Math/pow (- a b) 2))
                              point1
                              point2))))

(defn make-pairs [points & {:keys [sort-pairs?] :or {sort-pairs? true}}]
  (let [pairs (vec (for [i (range (count points))
                         j (range (inc i) (count points))]
                     {:pairs [i j]
                      :dist (euclidean-distance (points i) (points j))}))]
    (if sort-pairs?
      (sort-by :dist pairs)
      pairs)))

(defn dsu-find [parent x]
  (if (= (parent x) x)
    {:parent parent
     :root x}
    (let [{:keys [parent root]} (dsu-find parent (parent x))]
      {:parent (assoc parent x root)
       :root root})))

(defn dsu-union [parent x y]
  (let [{parent :parent
         root-x :root} (dsu-find parent x)
        {parent :parent
         root-y :root} (dsu-find parent y)]
    (if (= root-x root-y)
      {:parent parent :joined? false}
      {:parent (assoc parent root-y root-x)
       :joined? true})))

(defn connect-points
  ([points num-connections] (connect-points points
                                            num-connections
                                            (make-pairs points)
                                            (vec (range (count points)))))
  ([points num-connections pairs connections]
   (if (zero? num-connections)
     connections
     (let [closest-pair (first pairs)
           remaining-pairs (rest pairs)
           [i j] (:pairs closest-pair)
           new-connections (:parent (dsu-union connections i j))]
       (recur points (dec num-connections) remaining-pairs new-connections)))))

(defn find-last-pair-to-form-one-comp
  ([points] (find-last-pair-to-form-one-comp points
                                             (make-pairs points)
                                             (vec (range (count points)))
                                             (count points)
                                             nil))
  ([points pairs connections num-components prev-pair]
   (if (= num-components 1)
     prev-pair
     (let [closest-pair (first pairs)
           remaining-pairs (rest pairs)
           [i j] (:pairs closest-pair)
           {new-connections :parent
            joined? :joined?} (dsu-union connections i j)
           new-num-components (if joined?
                                (dec num-components)
                                num-components)]
       (recur points remaining-pairs new-connections new-num-components (:pairs closest-pair))))))

(defn get-top-3-clusters [connections]
  (let [cluster-sizes (reduce (fn [acc conn]
                                (let [{:keys [root]} (dsu-find connections conn)]
                                  (update acc root (fnil inc 0))))
                              {}
                              (range (count connections)))]
    (take 3 (reverse (sort (vals cluster-sizes))))))

(def sample-input (parse-input "2025/day8_sample.txt"))
(def real-input (parse-input "2025/day8_input.txt"))

(def sample-top-3 (-> sample-input
                      (connect-points 10)
                      (get-top-3-clusters)))
(def part1-sample-result (reduce * 1 sample-top-3))

(def real-top-3 (-> real-input
                    (connect-points 1000)
                    (get-top-3-clusters)))
(def part1-real-result (reduce * 1 real-top-3))

(def sample-last-pair (find-last-pair-to-form-one-comp sample-input))
;; multiply the x-coord of the last pair which connected all points
(def part2-sample-result (* ((sample-input (first sample-last-pair)) 0)
                            ((sample-input (second sample-last-pair)) 0)))
(def real-last-pair (find-last-pair-to-form-one-comp real-input))
;; multiply the x-coord of the last pair which connected all points
(def part2-real-result (* ((real-input (first real-last-pair)) 0)
                          ((real-input (second real-last-pair)) 0)))
