(ns day06
  (:require [clojure.string :as str]))

(defn solve
  [filename num-distinct-chars]
  (->> (slurp filename)
       (str/trim)
       (partition num-distinct-chars 1)
       (map set)
       (keep-indexed (fn [idx val]
                       (when (= num-distinct-chars (count val))
                         idx)))
       (first)
       (+ num-distinct-chars)))

(defn part1
  [filename]
  (solve filename 4))

(assert (= 5 (part1 "day06.example")))
(assert (= 6 (part1 "day06.example2")))
(assert (= 10 (part1 "day06.example3")))
(assert (= 11 (part1 "day06.example4")))
(assert (= 1262 (part1 "day06.input")))

(defn part2
  [filename]
  (solve filename 14))

(assert (= 23 (part2 "day06.example")))
(assert (= 23 (part2 "day06.example2")))
(assert (= 29 (part2 "day06.example3")))
(assert (= 26 (part2 "day06.example4")))
(assert (= 19 (part2 "day06.example5")))
(assert (= 3444 (part2 "day06.input")))
