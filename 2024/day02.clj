(ns day02
  (:require [clojure.string :as str]))


(defn parse-level
  [s]
  (->> (str/split s #" +")
       (map Integer/parseInt)))

(defn level-safe?
  [level]
  (let [partitioned (partition 2 1 level)]
    (or (every? (fn [[l r]] (<= 1 (- r l) 3 )) partitioned)
        (every? (fn [[l r]] (<= 1 (- l r) 3 )) partitioned))))

(defn part1
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map parse-level)
       (filter level-safe?)
       (count)))

(println (part1 "day02.input"))
