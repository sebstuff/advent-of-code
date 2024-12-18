(ns day18
  (:require [clojure.string :as str]))


(defn neighbors
  [[x y] size]
  (cond-> []
    (< 0 x) (conj [(dec x) y])
    (< 0 y) (conj [x (dec y)])
    (> (dec size) x) (conj [(inc x) y])
    (> (dec size) y) (conj [x (inc y)])))

(defn find-cost
  [size n-corrupted corrupted]
  (let [corrupted (set (take n-corrupted corrupted))
        exit [(dec size) (dec size)]]
    (loop [to-visit #{[0 0]}
           costs {[0 0] 0}]
      (let [visiting (first (sort-by costs to-visit))
            cost (costs visiting)]
        (cond
          (nil? visiting)
          nil

          (= exit visiting)
          cost

          :else
          (let [neighbors (->> (neighbors visiting size)
                               (filter #(not (corrupted %)))
                               (filter #(not (costs %))))
                to-visit' (-> to-visit
                              (disj visiting)
                              (into neighbors))
                costs' (reduce #(assoc %1 %2 (inc cost))
                               costs
                               neighbors)]
            (recur to-visit' costs')))))))

(defn part1
  [filename size n-corrupted]
  (->> (slurp filename)
       (str/split-lines)
       (mapv #(str/split % #","))
       (mapv (fn [[x y]] [(parse-long x) (parse-long y)]))
       (find-cost size n-corrupted)))

(assert (= 22 (part1 "day18.example" 7 12)))
(println (time (part1 "day18.input" 71 1024)))


(defn part2
  [filename size]
  (let [corrupted (->> (slurp filename)
                       (str/split-lines)
                       (mapv #(str/split % #","))
                       (mapv (fn [[x y]] [(parse-long x) (parse-long y)])))]
    (loop [lo-corrupted 0
           hi-corrupted (count corrupted)]
      (let [test-corrupted (quot (+ lo-corrupted hi-corrupted) 2)
            cost-test (find-cost size test-corrupted corrupted)
            cost-next-test (find-cost size (inc test-corrupted) corrupted)]
        (cond
          ;; both nil, we're too high
          (and (nil? cost-test)
               (nil? cost-next-test))
          (recur lo-corrupted test-corrupted)

          ;; neither nil, we're too low
          (and cost-test cost-next-test)
          (recur test-corrupted hi-corrupted)

          ;; just right
          :else
          (nth corrupted test-corrupted))))))

(assert (= [6 1] (part2 "day18.example" 7)))
(println (time (part2 "day18.input" 71)))
