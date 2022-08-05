(ns day15
  (:require [clojure.string :as string]))


(defn parse-input
  [input]
  (->> input
       string/split-lines
       (mapcat (fn [row line]
                 (map (fn [col risk]
                           [[row col] (- (int risk) (int \0))])
                         (range)
                         line))
               (range))
       (into {})))


(defn find-path
  [graph from to]
  (loop [node from
         visit-nodes #{from}
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
            visit-nodes' (disj (set (concat visit-nodes neighbors)) node)

            next-node (first (sort-by #(get paths-costs' %) visit-nodes'))]
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
