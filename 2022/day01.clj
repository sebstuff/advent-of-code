(ns day01
  (:require [clojure.string :as string]))

;;; parses an elf from a string
(defn- parse-elf
  [elf-str]
  (->> elf-str
       (string/split-lines)
       (transduce (map parse-long) + 0)))

;;; reads elves from a file
(defn read-elves
  [filename]
  (let [data (slurp filename)
        lines (string/split data #"\n\n")
        elves (map parse-elf lines)]
    elves))

;;; finds the total calories for the top elf
(defn part1
  [filename]
  (->> filename
      (read-elves)
      (sort >)
      (first)))

(assert (= 24000 (part1 "day01.example")))
(assert (= 69289 (part1 "day01.input")))

;;; finds the total calories for the top 3 elves
(defn part2
  [filename]
  (->> filename
       (read-elves)
       (sort >)
       (take 3)
       (apply +)))

(assert (= 45000 (part2 "day01.example")))
(assert (= 205615 (part2 "day01.input")))
