(ns day02
  (:require [clojure.string :as str]))

(defn parse-cubes
  [s]
  (let [[count color] (str/split (str/trim s) #" ")]
    [(keyword color) (parse-long count)]))

(defn parse-round
  [s]
  (into {} (mapv parse-cubes (str/split (str/trim s) #","))))

(defn parse-rounds
  [s]
  (mapv parse-round (str/split s #";")))

(defn parse-game [s]
  (let [[_ game-number rounds] (re-find #"Game (\d+): (.+)" s)
        game-number (parse-long game-number)]
    {:game-number game-number
     :rounds (parse-rounds rounds)}))

(defn parse-input [input]
  (->> input
      (str/split-lines)
      (map parse-game)))

(defn round-possible?
  [round cube-limit]
  (every? (fn [[color count]]
            (<= count (cube-limit color)))
          round))

(defn game-possible?
  [{:keys [rounds]} cube-limit]
  (every? #(round-possible? % cube-limit) rounds))

(defn part1 [filename]
  (let [cube-limit {:red 12 :green 13 :blue 14}
        input (parse-input (slurp filename))]
    (->> input
         (filter #(game-possible? % cube-limit))
         (map :game-number)
         (apply +))))

(assert (= 8 (part1 "day02.example")))
(assert (= 2505 (part1 "day02.input")))

(println "Part 1:" (part1 "day02.input"))

(defn minimum-cubes
  [rounds]
  (reduce (fn [acc [color count]]
            (if (contains? acc color)
              (assoc acc color (max count (acc color)))
              (assoc acc color count)))
          {}
          (apply concat rounds)))

(defn calculate-power
  [cubes]
  (reduce * (map second cubes)))

(defn part2 [filename]
  (let [input (parse-input (slurp filename))]
    (->> input
         (map :rounds)
         (map minimum-cubes)
         (map calculate-power)
         (apply +))))

(assert (= 2286 (part2 "day02.example")))
(assert (= 70265 (part2 "day02.input")))

(println "Part 2:" (part2 "day02.input"))
