(ns day01
  (:require [clojure.string :as str]
            [clojure.core.reducers :as r]))

(->> (slurp "day01.input")
     (str/split-lines)
     (map #(Integer/parseInt %))
     (partition 2 1)
     (filter (fn [[first second]] (> second first)))
     (count)
     (time)
     (println "Part 1:"))

(->> (slurp "day01.input")
     (str/split-lines)
     (map #(Integer/parseInt %))
     (partition 3 1)
     (map #(apply + %))
     (partition 2 1)
     (filter (fn [[first second]] (> second first)))
     (count)
     (println "Part 2:"))

(comment
  (def nums
    (vec
     (let [random (java.util.Random. 2021)]
       (repeatedly 1000000 #(+ 10 (.nextInt random 90))))))

  (sliding)
  ;; 600ms
  (->> nums
       (partition 2 1)
       (filter (fn [[first second]] (> second first)))
       (count)
       (time)
       (println "partition, filter, count:"))

  ;; 193ms
  (->> (map < nums (rest nums))
       (filter identity)
       (count)
       (time)
       (println "map, filter, count:"))

  ;; 136ms
  (->> (map < nums (rest nums))
       (reduce (fn [sofar v] (if v (inc sofar) sofar)) 0)
       (time)
       (println "map, reduce:"))

  ;; 148ms
  (->> (map < nums (rest nums))
       (transduce
        (map #(if % 1 0))
        +
        0)
       (time)
       (println "map, transduce:"))

  (time (count (into [] (r/map inc nums))))
  (time (count (into [] (map inc nums))))
  (time (count (map inc nums)))

  )
