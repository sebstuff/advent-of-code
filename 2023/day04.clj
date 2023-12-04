(ns day04
  (:require [clojure.string :as str]))

(defn parse-card
  [line]
  (let [[_ card-number winning-numbers picks] (re-find #"Card +(\d+): +([\d ]+) +\| +([\d ]+)" line)
        card-number (parse-long card-number)
        winning-numbers (map parse-long (str/split winning-numbers #" +"))
        picks (mapv parse-long (str/split picks #" +"))]
    {:card-number card-number
     :winning-numbers winning-numbers
     :picks picks}))

(defn score-card
  [{:keys [winning-numbers picks]}]
  (let [matches (count (clojure.set/intersection (set winning-numbers) (set picks)))]
    (long (Math/pow 2 (dec matches)))))

(defn part1
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (mapv parse-card)
       (mapv score-card)
       (apply +)))

(assert (= 13 (part1 "day04.example")))
(assert (= 18619 (part1 "day04.input")))
