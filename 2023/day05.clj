(ns day05
  (:require [clojure.string :as str]))

(defn ->range
  [start count]
  [start count])

(defn in-range?
  [[start count] n]
  (<= start n (+ start count)))
(assert true (in-range? (->range 0 3) 0))
(assert true (in-range? (->range 0 3) 1))
(assert true (in-range? (->range 0 3) 2))
(assert true (in-range? (->range 0 3) 3))

(defn translate-to-range
  [from-range to-range n]
  (+ (first to-range) (- n (first from-range))))
(assert (= 4 (translate-to-range (->range 0 3) (->range 4 3) 0)))

(defn lookup
  [{:keys [ranges]} n]
  (let [range (first (filter #(in-range? (:from %) n) ranges))]
    (if (empty? range)
      n
      (translate-to-range (:from range) (:to range) n))))

(defn parse-sparse-map
  [s];[_ name & rows]
  (let [[_ name _ ranges] (re-find #"([a-zA-Z-]+)( map)?:\n?([\d\n\r ]+)+$" s)
        [_ prev-name next-name] (re-find #"([a-zA-Z]+)-to-([a-zA-Z]+)" name)
        ranges (mapv (fn parse-row[r]
                       (let [[_ to from count] (re-find #"(\d+) +(\d+) +(\d+)" r)
                             from (parse-long from)
                             to (parse-long to)
                             count (parse-long count)]
                         {:from (->range from count)
                          :to (->range to count)}))
                     (str/split ranges #"\n"))]
    {:name name :prev-name prev-name :next-name next-name :ranges ranges}))

(defn seed-location
  [sparse-maps seed]
  (loop [map-idx 0
         value seed]
    (if (>= map-idx (count sparse-maps))
      value
      (let [sparse-map (nth sparse-maps map-idx)]
        (recur (inc map-idx) (lookup sparse-map value))))))

(defn part1
  [filename]
  (let [[seeds & sparse-map-strs] (-> (slurp filename)
                                      (str/split #"\n\n"))
        seeds (mapv parse-long (re-seq #"\d+" seeds))
        sparse-maps (mapv parse-sparse-map
                          sparse-map-strs)]
    (->> seeds
         (mapv #(seed-location sparse-maps %))
         (apply min))))

(assert (= 35 (part1 "day05.example")))
(time (assert (= 403695602 (part1 "day05.input"))))

(defn reverse-lookup
  [{:keys [ranges]} n]
  (let [range (first (filter #(in-range? (:to %) n) ranges))]
    (if (empty? range)
      n
      (translate-to-range (:to range) (:from range) n))))

(defn location-seed
  [sparse-maps location]
  (loop [map-idx (dec (count sparse-maps))
         value location]
    (if (< map-idx 0)
      value
      (let [sparse-map (nth sparse-maps map-idx)]
        (recur (dec map-idx) (reverse-lookup sparse-map value))))))

(defn seed?
  [seeds seed]
  (some (fn [[start count]] (<= start seed (+ start count)))
        (partition 2 seeds)))

(defn part2
  [filename]
  (let [[seeds & sparse-map-strs] (-> (slurp filename)
                                      (str/split #"\n\n"))
        seeds (mapv parse-long (re-seq #"\d+" seeds))
        sparse-maps (mapv parse-sparse-map
                          sparse-map-strs)]
    (loop [location 0]
      (let [seed (location-seed sparse-maps location)]
        (when (and (not= 0 location) (= 0 (mod location 1000000)))
          (println "location" location "seed" seed))
        (if (seed? seeds seed)
          location
          (recur (inc location)))))))

(assert (= 46 (part2 "day05.example")))
(time (assert (= 219529182 (part2 "day05.input"))))
