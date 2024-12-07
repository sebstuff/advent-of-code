(ns day07
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combos]))

(defn parse-equation
  [s]
  (let [[test-value-str nums-str] (str/split s #": ")]
    {:test-value (bigint test-value-str)
     :nums (->> (str/split nums-str #" ")
                (mapv bigint))}))

(defn parse-input
  [s]
  (->> (str/split-lines s)
       (mapv parse-equation)))

(defn apply-operations
  [equation operations]
  (let [{:keys [nums]} equation]
    (loop [[num & nums-rest] (rest nums)
           [operation & operations] operations
           acc (first nums)]
      (if num
        (recur nums-rest operations (operation acc num))
        acc))))

(defn solvable?
  [equation valid-operations]
  (let [{:keys [test-value nums]} equation
        num-operations (dec (count nums))
        ;; kinda hacky. In order to use permuated-combinations we need to have the list
        ;; duplicate each operation based upon the number of times we might use it - e.g.
        ;; if there are 4 nums, we need this list to have 3 of each passed in valid-operation
        hacky-valid-operations (reduce (fn [sofar operation]
                                         (concat sofar (repeat num-operations operation)))
                                       []
                                       valid-operations)
        operation-combos (combos/permuted-combinations hacky-valid-operations num-operations)]
    (some (fn operations-solves-equation?[operations]
            (= test-value (apply-operations equation operations)))
          operation-combos)))

(defn total-calibration-result
    [equations valid-operations]
    (->> equations
         (filter #(solvable? % valid-operations))
         (map :test-value)
         (reduce + 0)))

(defn part1
  [filename]
  (->> (slurp filename)
       (parse-input)
       (#(total-calibration-result % [* +]))))

(println (time (part1 "day07.input")))


(defn ||
  [l r]
  (bigint (str l r)))

(defn part2
  [filename]
  (->> (slurp filename)
       (parse-input)
       (#(total-calibration-result % [* + ||]))))

(println (time (part2 "day07.input")))
