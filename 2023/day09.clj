(ns day09
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn find-differences
  [history]
  (loop [differences [history]]
    (let [next-difference (mapv (fn [[a b]] (- b a))
                                (partition 2 1 (last differences)))]
      (if (every? #(= % 0) next-difference)
        (conj differences next-difference)
        (recur (conj differences next-difference))))))

(defn predict-next
  [history]
  (let [differences (find-differences history)]
    (->> differences
         (map last)
         (apply +))))

(defn part1
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (mapv #(mapv parse-long (str/split % #" ")))
       (map predict-next)
       (apply +)))

(assert (= 114 (time (part1 "day09.example"))))
(assert (= 2005352194 (time (part1 "day09.input"))))


(defn predict-prev
  [history]
  (let [differences (find-differences history)]
    (->> differences
         (map first)
         (map-indexed (fn [idx val] (if (even? idx) val (- val))))
         (apply +))))

(defn part2
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (mapv #(mapv parse-long (str/split % #" ")))
       (map predict-prev)
       (apply +)))

(assert (= 2 (time (part2 "day09.example"))))
(assert (= 1077 (time (part2 "day09.input"))))
