(ns day04
  (:require [clojure.string :as str]))


(defn parse-input
  [input]
  (->> input
       (str/split-lines)
       (mapv vec)))

(defn count-xmas-at
  [sofar data spot]
  (if (= \X (get-in data spot))
    (loop [[direction & directions] (for [dr (range -1 2)
                                          dc (range -1 2)
                                          :when (not= 0 dr dc)]
                                      [dr dc])
           total sofar]
      (if direction
        (if (and (= \M (get-in data
                               (map #(+ %1 (* 1 %2)) spot direction)))
                 (= \A (get-in data
                               (map #(+ %1 (* 2 %2)) spot direction)))
                 (= \S (get-in data
                               (map #(+ %1 (* 3 %2)) spot direction))))
          (recur directions (inc total))
          (recur directions total))
        total))
    sofar))

(defn xmas-reduce
  "Version of reduce that works on our xmas structure."
  [f val data]
  (loop [[spot & spots] (for [r (range (count data))
                              c (range (count (first data)))]
                          [r c])
         result val]
    (if spot
      (recur spots (f result data spot))
      result)))


(defn part1
  [filename]
  (->> (slurp filename)
       (parse-input)
       (xmas-reduce count-xmas-at 0)))

(println (part1 "day04.input"))

(defn count-mas-x-at
  [sofar data [r c :as spot]]
  (if (and (= \A (get-in data spot))
           (= #{\M \S} (set [(get-in data [(dec r) (dec c)])
                             (get-in data [(inc r) (inc c)])]))
           (= #{\M \S} (set [(get-in data [(dec r) (inc c)])
                             (get-in data [(inc r) (dec c)])])))
    (inc sofar)
    sofar))

(defn part2
  [filename]
  (->> (slurp filename)
       (parse-input)
       (xmas-reduce count-mas-x-at 0)))

(println (part2 "day04.input"))
