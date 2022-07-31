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

(def ^:dynamic *single-visit* true)

(defn can-visit?
  [path node]
  (cond
    (= "start" node)
    false

    (every? #(Character/isUpperCase %) node)
    true

    :default
    (not (some #{node} path))))


(defn can-visit2?
  [path node]
  (cond
    (= "start" node)
    false

    (every? #(Character/isUpperCase %) node)
    true

    (not (some #{node} path))
    true

    :default
    (< (count (filter #(= node %) path)) 2)))

(defn expand-path
  [nodes path]
  (let [at (last path)
        neighbors (get nodes at)
        double-visited? (:double-visited? (meta path))
        can-visit-fn (if (or *single-visit* double-visited?) can-visit? can-visit2?)]
    (if (not= "end" at)
      (for [neighbor neighbors
            :when (can-visit-fn path neighbor)]
        (let [new-double-visited? (cond
                                    *single-visit*
                                    false

                                    double-visited?
                                    true

                                    (every? #(Character/isUpperCase %) neighbor)
                                    false

                                    :default
                                    (not (nil? (some #{neighbor} path))))]
          (-> path
              (conj neighbor)
              (vec)
              (with-meta {:double-visited? new-double-visited?}))))
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

(defn part2
  [filename]
  (binding [*single-visit* false]
    (-> filename
        slurp
        parse-input
        all-combos
        count)))

(assert (= 36 (part2 "day12.example")))
(assert (= 103 (part2 "day12.example2")))
(assert (= 3509 (part2 "day12.example3")))
(println "part2" (part2 "day12.input"))
