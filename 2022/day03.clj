(ns day03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn sack-compartments
  [sack]
  (map set (partition (/ (count sack) 2) sack)))

(defn item-priority
  [item]
  (if (Character/isUpperCase item)
    (+ 27 (- (int item) (int \A)))
    (+ 1 (- (int item) (int \a)))))

(defn load-sacks
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map vec)))

(defn part1
  [filename]
  (->> (load-sacks filename)
       (map sack-compartments) ; split each sack into two compartments
       (map #(set/intersection (first %) (second %))) ; combine compartment pairs via set intersection
       (map first)
       (map item-priority)
       (apply +)))

(assert (= 157 (part1 "day03.example")))
(assert (= 8105 (part1 "day03.input")))

(defn part2
  [filename]
  (->> (load-sacks filename)
       (partition 3 3) ; group the sacks into groups of 3
       (map (fn [group] (map set group))) ; convert each sack into a set
       (map #(apply set/intersection %)) ; combine grouped sacks via set intersection
       (map first)
       (map item-priority)
       (apply +)))

(assert (= 70 (part2 "day03.example")))
(assert (= 2363 (part2 "day03.input")))
