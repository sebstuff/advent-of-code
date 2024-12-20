(ns day20
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn neighbors
  [[x y] [mx my]]
  (cond-> []
    (< 0 x) (conj [(dec x) y])
    (< 0 y) (conj [x (dec y)])
    (> mx x) (conj [(inc x) y])
    (> my y) (conj [x (inc y)])))

(defn parse-grid
  [s]
  (let [lines (str/split-lines s)]
    (reduce (fn [grid pos]
              (case (get-in lines pos)
                \. (assoc grid pos {:type :track})
                \# (assoc grid pos {:type :wall})
                \S (assoc grid pos {:type :start})
                \E (assoc grid pos {:type :end})))
            {}
            (for [x (range (count (first lines)))
                  y (range (count lines))]
              [x y]))))

(defn max-pos
  [grid]
  [(apply max (map first (keys grid)))
   (apply max (map second (keys grid)))])

(defn populate-picos
  [grid]
  (let [end-pos (ffirst (filter (fn [[_pos cell]] (= :end (:type cell)))
                                grid))
        max-pos (max-pos grid)]
    (loop [[visit & visited] [end-pos]
           grid' (assoc-in grid [end-pos :picos] 0)]
      (if (not visit)
        grid'
        (let [neighbors (->> (neighbors visit max-pos)
                             (filter grid')
                             (filter #(not= :wall (get-in grid' [% :type])))
                             (filter #(not (get-in grid' [% :picos]))))
              visited' (into visited neighbors)
              grid'' (reduce (fn [sofar pos]
                               (assoc-in sofar [pos :picos] (inc (get-in grid' [visit :picos]))))
                             grid'
                             neighbors)]
          (recur visited' grid''))))))

(defn wallhack-pico-savings
  "Calculates the picos saved for a wallhack at the position."
  [grid pos]
  (let [max-pos (max-pos grid)
        start-pos (ffirst (filter (fn [[_pos cell]] (= :start (:type cell)))
                                  grid))
        start-picos (get-in grid [start-pos :picos])
        neighbors (->> (neighbors pos max-pos)
                       (filter #(not= :wall (get-in grid [% :type]))))]
    (if (> 2 (count neighbors))
      -2 ; moving in and out of a wallhack costs an extra 2 picos
      (let [neighbor-picos (map #(get-in grid [% :picos]) neighbors)
            max-neighbor-picos (apply max neighbor-picos)
            min-neighbor-picos (apply min neighbor-picos)]
        (+ -2 (- max-neighbor-picos min-neighbor-picos))))))

(defn all-wallhack-pico-savings
  [grid]
  (->> grid
       (filter (fn [[pos cell]] (= :wall (:type cell))))
       (map first)
       (pmap #(wallhack-pico-savings grid %))
       (filter #(< 0 %))))

(defn part1
  [filename]
  (let [grid (parse-grid (slurp filename))
        grid (populate-picos grid)]
    (->> grid
         (all-wallhack-pico-savings)
         (filter #(<= 100 %))
         (count))))

(println (time (part1 "day20.input")))
(shutdown-agents)
