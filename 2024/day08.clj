(ns day08
  (:require [clojure.string :as str]))

(defn parse-grid
  [input]
  (->> (str/split-lines input)
       (mapv vec)))

(defn grid->antenna-groups
  [grid]
  (reduce (fn collect-antennas[antenna-groups pos]
            (let [elem (get-in grid pos)]
              (if (= \. elem)
                antenna-groups
                (update antenna-groups elem conj pos))))
          {}
          (for [row (range (count grid))
                col (range (count (first grid)))]
            [row col])))

;; you need to bind this to your function for finding an antenna's antinodes
(def ^:dynamic *antennas-antinodes-fn* nil)

(defn antenna-groups-antinodes
  [antenna-groups rows cols]
  (reduce (fn [antinodes-acc antennas]
            (into antinodes-acc (*antennas-antinodes-fn* antennas rows cols)))
          #{}
          (vals antenna-groups)))

(defn solve
  [filename]
  (when-not *antennas-antinodes-fn*
    (throw (ex-info "You must first use binding to assign *antennas-antinodes-fn*." {})))

  (let [grid (parse-grid (slurp filename))
        rows (count grid)
        cols (count (first grid))
        antenna-groups (grid->antenna-groups grid)
        antinodes (antenna-groups-antinodes antenna-groups rows cols)]
    (count antinodes)))

(defn antennas-antinodes-part1[antennas rows cols]
    (let [antenna-pairs (for [a1 antennas
                              a2 antennas
                              :when (not= a1 a2)]
                          [a1 a2])]
      (reduce (fn add-antinodes[antinodes-acc [a1 a2]]
                (let [diff (map - a1 a2)
                      [arow acol :as antinode] (map + a1 diff)]
                  (if (and (< -1 arow rows)
                           (< -1 acol cols))
                    (conj antinodes-acc (map + a1 diff))
                    antinodes-acc)))
              #{}
              antenna-pairs)))

(defn part1
  [filename]
  (binding [*antennas-antinodes-fn* antennas-antinodes-part1]
    (solve filename)))

(println (time (part1 "day08.input")))

(defn antennas-antinodes-part2[antennas rows cols]
    (let [antenna-pairs (for [a1 antennas
                              a2 antennas
                              :when (not= a1 a2)]
                          [a1 a2])]
      (reduce (fn add-antinodes[antinodes-acc [a1 a2]]
                (let [diff (map - a1 a2)]
                  (loop [pos a1
                         antinodes-acc' antinodes-acc]
                    (let [[row col :as antinode] (map + pos diff)]
                      (if (and (< -1 row rows)
                               (< -1 col cols))
                        (recur antinode
                               (conj antinodes-acc' antinode))
                        antinodes-acc')))))
              (set antennas)
              antenna-pairs)))

(defn part2
  [filename]
  (binding [*antennas-antinodes-fn* antennas-antinodes-part2]
    (solve filename)))

(println (time (part2 "day08.input")))
