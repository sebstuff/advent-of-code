(ns day01
  (:require [clojure.string :as str]))


(defn parse-file
  "Parses contents of file into a list of pairs of numbers."
  [filename]
  (->> (slurp filename)
       ;; convert to list of pairs
       (str/split-lines)
       (map (fn [line] (str/split line #" +")))
       ;; convert strings to numbers
       (map (fn [[l r]] [(Integer/parseInt l) (Integer/parseInt r)]))))

(defn part1
  [filename]
  (let [data (->> (parse-file filename)
                  ;; convert from list of pairs to two lists
                  (reduce (fn [[sofar-l sofar-r] [l r]] [(conj sofar-l l) (conj sofar-r r)])
                          [[][]])
                  ;; sort the two lists
                  (map sort)
                  ;; convert from two sorted lists to list of pairs
                  (#(map (fn [l r] [l r]) (first %) (second %)))
                  ;; subtract pairs
                  (map #(Math/abs (apply - %)))
                  ;; add 'em all up
                  (apply +))]
    data))
(println (part1 "day01.input"))

(defn part2
  [filename]
  (let [data (->> (parse-file filename)
                  ;; convert from list of pairs to two lists
                  (reduce (fn [[sofar-l sofar-r] [l r]] [(conj sofar-l l) (conj sofar-r r)])
                          [[][]])
                  ;; calculate similarity score
                  ((fn [[l-list r-list]] (map (fn [l] (* l (count (filter #(= l %) r-list)))) l-list)))
                  ;; add 'em all up
                  (apply +))]
    data))
(println (part2 "day01.input"))
