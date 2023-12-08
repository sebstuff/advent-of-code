(ns day08
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(def start-node "AAA")
(def end-node "ZZZ")

(defn parse-input
  [s]
  (let [[_ instructions] (re-find #"^([RL]+)\n\n" s)
        nodes (->> (re-seq #"([A-Z]{3,3}) = \(([A-Z]{3,3}), ([A-Z]{3,3})\)" s)
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
