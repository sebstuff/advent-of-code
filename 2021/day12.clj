(ns day12
 (:require [clojure.string :as string]))


(defn parse-input
  [input]
  (->> input
       string/split-lines
       (map #(string/split % #"-"))
       (reduce (fn [m [f s]]
                 (-> m
                     (update f conj s)
                     (update s conj f)))
               {})))


(defn can-visit?
  [path node]
  (cond
    (= "start" node)
    false

    (not (some #{node} path))
    true

    :default
    (every? #(Character/isUpperCase %) node)))


(defn expand-path
  [nodes path]
  (let [at (last path)
        neighbors (get nodes at)]
    (if (not= "end" at)
      (->> neighbors
           (filter #(can-visit? path %))
           (map #(conj path %))
           vec)
      [path])))


(defn all-combos
  ([nodes] (all-combos nodes [["start"]]))
  ([nodes paths]
   (let [expanded-paths (into [] (mapcat #(expand-path nodes %) paths))]
     (if (every? #(= "end" (last %)) expanded-paths)
       expanded-paths
       (recur nodes expanded-paths)))))


(defn part1
  [filename]
  (-> filename
      slurp
      parse-input
      all-combos
      count))


(assert (= 10 (part1 "day12.example")))
(assert (= 19 (part1 "day12.example2")))
(assert (= 226 (part1 "day12.example3")))
(println "part1" (part1 "day12.input"))
