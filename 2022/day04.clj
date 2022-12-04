(ns day04
  (:require [clojure.string :as str]))

(defn parse-range
  [r]
  (map parse-long (str/split r #"-")))

(defn parse-pair
  [pair]
  (map parse-range (str/split pair #",")))

(defn fully-contains?
  [r1 r2]
  (let [lower1 (first r1)
        lower2 (first r2)
        upper1 (second r1)
        upper2 (second r2)]
    (and (<= lower1 lower2) (>= upper1 upper2))))

(defn part1
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map parse-pair)
       (filter #(or (fully-contains? (first %) (second %)) (fully-contains? (second %) (first %))))
       (count)))

(assert (= 2 (part1 "day04.example")))
(assert (= 456 (part1 "day04.input")))

(defn between?
  [n1 n2 n3]
  (and (<= n1 n2) (>= n3 n2)))

(defn overlaps?
  [r1 r2]
  (let [lower1 (first r1)
        lower2 (first r2)
        upper1 (second r1)
        upper2 (second r2)]
    (or (between? lower1 lower2 upper1)
        (between? lower1 upper2 upper1))))

(defn part2
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map parse-pair)
       (filter #(or (overlaps? (first %) (second %)) (overlaps? (second %) (first %))))
       (count)))

(assert (= 4 (part2 "day04.example")))
(assert (= 808 (part2 "day04.input")))
