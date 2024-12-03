(ns day02
  (:require [clojure.string :as str]))


(defn parse-level
  [s]
  (->> (str/split s #" +")
       (map Integer/parseInt)))

(defn level-safe?
  [level]
  (let [pairs (partition 2 1 level)]
    (or (every? (fn [[l r]] (<= 1 (- r l) 3 )) pairs)
        (every? (fn [[l r]] (<= 1 (- l r) 3 )) pairs))))

(defn part1
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map parse-level)
       (filter level-safe?)
       (count)))

(println (part1 "day02.input"))

(defn dampened-level-safe?
  [level]
  (let [pairs (partition 2 1 level)
        safe-neg? (fn [[l r]] (<= 1 (- r l) 3 ))
        safe-pos? (fn [[l r]] (<= 1 (- l r) 3 ))
        unsafe-neg? (fn [pair] (not (safe-neg? pair)))
        unsafe-pos? (fn [pair] (not (safe-pos? pair)))
        unsafe-neg (count (filter unsafe-neg? pairs))
        unsafe-pos (count (filter unsafe-pos? pairs))]
    (cond
      (= 0 unsafe-neg)
      true

      (= 0 unsafe-pos)
      true

      (and (or (<= 2 unsafe-neg) (<= 2 unsafe-pos))
           (let [dampened-pairs (for [x (range (count level))
                                      :let [level' (concat (take x level) (drop (inc x) level))]]
                                  (partition 2 1 level'))]
             (or (some #(every? safe-neg? %) dampened-pairs)
                 (some #(every? safe-pos? %) dampened-pairs))))

      true

      :else
      false)))

(defn part2
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map parse-level)
       (filter dampened-level-safe?)
       (count)))

(println (part2 "day02.input"))
