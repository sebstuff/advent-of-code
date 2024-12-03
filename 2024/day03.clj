(ns day03
  (:require [clojure.string :as str]))


(defn part1
  [filename]
  (->> (slurp filename)
       (re-seq #"mul\(\d+,\d+\)")
       (map #(re-matches #"mul\((\d+),(\d+)\)" %))
       (map #(drop 1 %))
       (map #(map Integer/parseInt %))
       (map #(apply * %))
       (apply +)))

(println (part1 "day03.input"))


(defn part2
  [filename]
  (->> (slurp filename)
       (#(str/replace % #"\r|\n" ""))
       (#(str/replace % #"don't\(\).*?do\(\)" ""))
       (#(str/replace % #"don't\(\).*" "")) ; incase there's a don't with no matching do
       (re-seq #"mul\(\d+,\d+\)")
       (map #(re-matches #"mul\((\d+),(\d+)\)" %))
       (map #(drop 1 %))
       (map #(map Integer/parseInt %))
       (map #(apply * %))
       (apply +)))

(println (part2 "day03.input"))
