(ns day08
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.math.numeric-tower :refer [lcm]]))

(def start-node "AAA")
(def end-node "ZZZ")

(defn parse-input
  [s]
  (let [[_ instructions] (re-find #"^([RL]+)\n\n" s)
        nodes (->> (re-seq #"([A-Z0-9]{3,3}) = \(([A-Z0-9]{3,3}), ([A-Z0-9]{3,3})\)" s)
                   (map (fn [[_ node left right]]
                          {:node node
                           :left left
                           :right right}))
                   (map (juxt :node identity))
                   (into {}))]
    {:instructions instructions :nodes nodes}))

(defn walk
  [{:keys [instructions nodes]}]
  (loop [[instruction & instructions'] instructions
         on-node (get nodes start-node)
         steps 1]
    (let [next-node-name (if (= \R instruction)
                           (:right on-node)
                           (:left on-node))
          next-node (get nodes next-node-name)]
      (if (= next-node-name end-node)
        steps
        (let [instructions' (if (empty? instructions')
                              instructions
                              instructions')]
          (recur instructions' next-node (inc steps)))))))

(defn part1
  [filename]
  (-> (slurp filename)
      (parse-input)
      (walk)))

(assert (= 2 (time (part1 "day08.example"))))
(assert (= 16271 (time (part1 "day08.input"))))


(defn walk2
  [{:keys [instructions nodes]} start-node]
  (loop [[instruction & instructions'] instructions
         on-node (get nodes start-node)
         steps 1]
    (let [next-node-name (if (= \R instruction)
                           (:right on-node)
                           (:left on-node))
          next-node (get nodes next-node-name)]
      (if (str/ends-with? next-node-name "Z")
        steps
        (let [instructions' (if (empty? instructions')
                              instructions
                              instructions')]
          (recur instructions' next-node (inc steps)))))))

;;; This works by finding the step count for each start node, then finding the least common multiple of all of them.
(defn part2
  [filename]
  (let [{:keys [nodes] :as input} (parse-input (slurp filename))
        start-nodes (filterv #(str/ends-with? % "A") (keys nodes))
        start-node-steps (mapv #(walk2 input %) start-nodes)]
    (reduce lcm 1 start-node-steps)))

(assert (= 6 (time (part2 "day08.example2"))))
(assert (= 14265111103729 (time (part2 "day08.input"))))
