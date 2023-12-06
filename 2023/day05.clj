(ns day05
  (:require [clojure.string :as str]))

(defn ->range
  [start count]
  [start (+ start (dec count)) count])

(defn in-range?
  [[start end count] n]
  (<= start n end))
(assert (in-range? (->range 0 3) 0))
(assert (in-range? (->range 0 3) 1))
(assert (in-range? (->range 0 3) 2))
(assert (not (in-range? (->range 0 3) 3)))

(defn translate-to-range
  [from-range to-range n]
  (+ (first to-range) (- n (first from-range))))
(assert (= 4 (translate-to-range (->range 0 3) (->range 4 3) 0)))

(defn ->rule
  [from to count]
  {:from (->range from count)
   :to (->range to count)})

(defn ->ruleset
  [name rules]
  {:name name :rules (vec (sort-by :from rules))})

(defn rule-for-number
  [ruleset n]
  (first (filter #(in-range? (:from %) n) ruleset)) )

(defn lookup
  [{:keys [rules]} n]
  (let [rule (rule-for-number rules n)]
    (if (empty? rule)
      n
      (translate-to-range (:from rule) (:to rule) n))))

(defn parse-ruleset
  [s]
  (let [[_ name _ rules] (re-find #"([a-zA-Z-]+)( map)?:\n?([\d\n\r ]+)+$" s)
        rules (map (fn parse-row[r]
                     (let [[_ to from count] (re-find #"(\d+) +(\d+) +(\d+)" r)]
                       (->rule (parse-long from) (parse-long to) (parse-long count))))
                   (str/split rules #"\n"))]
    (->ruleset name rules)))

(defn apply-rulesets
  [rulesets n]
  (loop [map-idx 0
         value n]
    (if (>= map-idx (count rulesets))
      value
      (let [ruleset (nth rulesets map-idx)]
        (recur (inc map-idx) (lookup ruleset value))))))

(defn part1
  [filename]
  (let [[seeds & ruleset-strs] (-> (slurp filename)
                                      (str/split #"\n\n"))
        seeds (mapv parse-long (re-seq #"\d+" seeds))
        rulesets (mapv parse-ruleset
                          ruleset-strs)]
    (->> seeds
         (mapv #(apply-rulesets rulesets %))
         (apply min))))

(assert (= 35 (part1 "day05.example")))
(time (assert (= 403695602 (part1 "day05.input"))))

(defn invert-rule [rule]
  (let [{:keys [from to]} rule]
    {:from to :to from}))

(defn invert-ruleset
  [ruleset]
  (update ruleset :rules #(mapv invert-rule %) ))

(defn seed?
  [seeds seed]
  (some (fn [[start count]] (<= start seed (+ start count)))
        (partition 2 seeds)))

(defn part2
  [filename]
  (let [[seeds & ruleset-strs] (-> (slurp filename)
                                   (str/split #"\n\n"))
        seeds (mapv parse-long (re-seq #"\d+" seeds))
        rulesets (->> ruleset-strs
                      (mapv parse-ruleset)
                      (reverse)
                      (mapv invert-ruleset))]
    (loop [location 0]
      (let [seed (apply-rulesets rulesets location)]
        (when (and (not= 0 location) (= 0 (mod location 1000000)))
          (println "location" location "seed" seed))
        (if (seed? seeds seed)
          location
          (recur (inc location)))))))

(assert (= 46 (part2 "day05.example")))
(time (assert (= 219529182 (part2 "day05.input"))))
