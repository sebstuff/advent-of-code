(ns day15
  (:require [clojure.string :as string]
            [clojure.data.priority-map :refer [priority-map]]))


(defn parse-input
  [input]
  (->> input
       string/split-lines
       (mapcat (fn [row line]
                 (map (fn [col risk]
                           [[col row] (- (int risk) (int \0))])
                         (range)
                         line))
               (range))
       (into {})))


(defn find-path
  [graph from to]
  (loop [node from
         visit-nodes (priority-map from 0)
         paths {from []}
         paths-costs {from 0}]
    (if (= to node)
      {:path (get paths to)
       :cost (get paths-costs to)}
      (let [cost (get paths-costs node)
            path (get paths node)
            prelim-neighbors (for [dx [-1 0 1]
                                   dy [-1 0 1]
                                   :let [neighbor [(+ dx (first node))
                                                   (+ dy (second node))]]
                                   :when (and (or (= 0 dx) (= 0 dy))
                                              (get graph neighbor)
                                              (not= node neighbor))]
                               neighbor)
            neighbor-costs (zipmap prelim-neighbors (map #(+ cost (get graph %)) prelim-neighbors))
            neighbors (filter #(< (get neighbor-costs %) (get paths-costs % Integer/MAX_VALUE)) prelim-neighbors)
            paths-costs' (merge paths-costs (select-keys neighbor-costs neighbors))
            paths' (reduce (fn [sofar neighbor] (assoc sofar neighbor (conj path neighbor))) paths neighbors)
            visit-nodes' (dissoc (into visit-nodes (select-keys neighbor-costs neighbors)) node)
            [next-node _] (first visit-nodes')]
        (recur next-node visit-nodes' paths' paths-costs')))))


(defn part1
  [filename]
  (let [graph (->> filename
                   slurp
                   parse-input)
        from [0 0]
        to [(apply max (map first (keys graph)))
            (apply max (map second (keys graph)))]
        answer (find-path graph from to)]
    answer))


(println "part1" (part1 "day15.input"))


(defn part2
  [filename]
  (let [graph (->> filename
                   slurp
                   parse-input)
        max-x (apply max (map first (keys graph)))
        max-y (apply max (map second (keys graph)))
        graph' (into {} (for [x (range (* 5 (inc max-x)))
                              y (range (* 5 (inc max-y)))]
                          (let [node [x y]
                                base-cost (get graph [(rem x (inc max-x)) (rem y (inc max-y))])
                                extra-cost (+ (int (/ x (inc max-x))) (int (/ y (inc max-x))))
                                cost (+ base-cost extra-cost)
                                clamped-cost (inc (rem (dec cost) 9))]
                            [node clamped-cost])))

        from [0 0]
        max-x' (apply max (map first (keys graph')))
        max-y' (apply max (map second (keys graph')))
        to [max-x' max-y']
        answer (find-path graph' from to)]
    answer))


(println "part2" (part2 "day15.input"))
