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

(defn solvable?
  [equation valid-operations]
  (let [{:keys [test-value nums]} equation]
    (letfn [(try-make-test-value [acc nums]
              ;; Try to make the test value.  If it fails, the value returned will be nonsense.  If it
              ;; succeeds, it will return the test value.
              ;;
              ;; This is recursively called, and loops over each valid operation.  If at any time the
              ;; potential answer is greater than the test value, it breaks out early.
              (if-let [num (first nums)]
                (loop [[operation & operations] valid-operations]
                  (if operation
                    (let [acc' (operation acc num)]
                      (if (<= acc' test-value)
                        (let [acc'' (try-make-test-value acc' (rest nums))]
                          (if (= acc'' test-value)
                            acc''
                            (recur operations)))
                        (recur operations)))
                    0))
                acc))]
      (= test-value (try-make-test-value (first nums) (rest nums))))))

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
