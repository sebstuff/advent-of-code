(ns day01
  (:require [clojure.string :as str]))

(defn part1 [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map (fn [line]
              (letfn [(digit? [c] (Character/isDigit c))]
                (let [first-digit (first (filter digit? line))
                      last-digit (first (filter digit? (reverse line)))]
                  (Integer/parseInt (str first-digit last-digit))))))
       (apply +)))

(assert (= 142 (part1 "day01.example")))
(assert (= 55621 (part1 "day01.input")))

(println "Part 1:" (part1 "day01.input"))

(defn part2 [filename]
  (letfn [(line->digit [line idx f]
            (let [ch (nth line idx)]
              (if (Character/isDigit ch)
                ch
                (let [substr (subs line idx)
                      word->digit {"one" 1
                                   "two" 2
                                   "three" 3
                                   "four" 4
                                   "five" 5
                                   "six" 6
                                   "seven" 7
                                   "eight" 8
                                   "nine" 9}]
                  (if-let [match (first (filter #(str/starts-with? substr %) (keys word->digit)))]
                    (word->digit match)
                    (recur line (f idx) f))))))]
    (->> (slurp filename)
        (str/split-lines)
        (map (fn [line]
               (let [first-digit (line->digit line 0 inc)
                     last-digit (line->digit line (dec (count line)) dec)]
                 (Integer/parseInt (str first-digit last-digit)))))
        (apply +))))


(assert (= 281 (part2 "day01.example2")))
(assert (= 53592 (part2 "day01.input")))
(println "Part 2:" (part2 "day01.input"))
