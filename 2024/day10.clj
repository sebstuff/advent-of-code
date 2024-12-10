(ns day10
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn parse-grid
  [input]
  (->> (str/split-lines input)
       (mapv vec)
       (mapv (fn [row] (mapv #(Integer/parseInt (str %)) row)))))

(defn coords-with-height
  [grid height]
  (for [row (range (count grid))
        col (range (count (first grid)))
        :when (= height (get-in grid [row col]))]
    [row col]))

(defn neighbors
  [[row col :as coord]]
  [[(dec row) col]
   [(inc row) col]
   [row (dec col)]
   [row (inc col)]])

(defn reachable-summits
  "Returns a map of coordinates to its height and the summits that coordinate can reach."
  [grid]
  (loop [height 9
         graph (transient {})]
    (case height
      -1
      (persistent! graph)
      
      9
      (recur (dec height)
             (reduce (fn assign-summit-values[graph' coord]
                       (assoc! graph' coord {:reachable-summits #{coord} :height height}))
                     graph
                     (coords-with-height grid height)))

      (recur (dec height)
             (reduce (fn assign-reachable[graph' coord]
                       (let [reachable-neighbors (->> (neighbors coord)
                                                      (filter #(= (inc height) (get-in grid %))))
                             reachable-summits (mapcat #(get-in graph' [% :reachable-summits]) reachable-neighbors)]
                         (assoc! graph' coord {:reachable-summits (set reachable-summits) :height height})))
                     graph
                     (coords-with-height grid height))))))

(defn trailhead-scores
  "Returns a map of trailhead coordinate to their score."
  [reachable-summits]
  (->> reachable-summits
       (filter (fn [[k v]] (= 0 (:height v))))
       (map (fn [[k v]] [k (count (:reachable-summits v))]))
       (into {})))

(defn part1
  [filename]
  (->> (slurp filename)
       (parse-grid)
       (reachable-summits)
       (trailhead-scores)
       (map second)
       (apply +)))

(pp/pprint (time (part1 "day10.input")))
