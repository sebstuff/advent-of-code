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

(defn card-matches
  [{:keys [winning-numbers picks]}]
  (count (clojure.set/intersection (set winning-numbers) (set picks))))

(defn card-score
  [card]
    (long (Math/pow 2 (dec (card-matches card)))))

(defn part1
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (mapv parse-card)
       (mapv card-score)
       (apply +)))

(assert (= 13 (part1 "day04.example")))
(time (assert (= 18619 (part1 "day04.input"))))

(defn part2
  [filename]
  (let [cards (->> (slurp filename)
                   (str/split-lines)
                   (mapv parse-card))]
    (loop [[card-count & card-counts] (repeat (count cards) 1)
           [card & cards] cards
           total-cards 0]
      (if-not card
        total-cards
        (let [matches (card-matches card)
              card-counts' (reduce (fn [acc n]
                                     (if (>= n (count acc))
                                       acc
                                       (update acc n #(+ % card-count))))
                                   (vec card-counts)
                                   (range 0 matches))]
          (recur card-counts' cards (+ total-cards card-count)))))))

(assert (= 30 (part2 "day04.example")))
(time (assert (= 8063216 (part2 "day04.input"))))
