(ns day07
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn hand-strength
  [hand wild?]
  (if wild?
    (let [card-matches (->> (frequencies hand)
                            (map reverse)
                            (sort-by first >))
          wildcards (first (filter #(= \J (second %)) card-matches))
          card-matches (filterv #(not= \J (second %)) card-matches)
          card-matches (if (nil? wildcards)
                         card-matches
                         (update card-matches 0 (fn apply-wildcards[[count card]]
                                                  (if (nil? count)
                                                    wildcards
                                                    [(+ count (first wildcards)) card]))))
          card-matches (map first card-matches)]
      (cond
        ;; 5 of a kind
        (= [5] card-matches)
        7

        ;; 4 of a kind
        (= [4 1] card-matches)
        6

        ;; full house
        (= [3 2] card-matches)
        5

        ;; 3 of a kind
        (= [3 1 1] card-matches)
        4

        ;; 2 pair
        (= [2 2 1] card-matches)
        3

        ;; 1 pair
        (= 2 (first card-matches))
        2

        ;; high card
        :else
        1))
    (let [card-matches (->> (frequencies hand)
                            (vals)
                            (sort >))]
      (cond
        ;; 5 of a kind
        (= [5] card-matches)
        7

        ;; 4 of a kind
        (= [4 1] card-matches)
        6

        ;; full house
        (= [3 2] card-matches)
        5

        ;; 3 of a kind
        (= [3 1 1] card-matches)
        4

        ;; 2 pair
        (= [2 2 1] card-matches)
        3

        ;; 1 pair
        (= 2 (first card-matches))
        2

        ;; high card
        :else
        1))))
(assert (= (hand-strength "AAAAA" false) 7))
(assert (= (hand-strength "KAAAA" false) 6))
(assert (= (hand-strength "AAKAA" false) 6))
(assert (= (hand-strength "AAAAK" false) 6))
(assert (= (hand-strength "AAAAK" false) 6))
(assert (= (hand-strength "AAAKK" false) 5))
(assert (= (hand-strength "AKAKA" false) 5))
(assert (= (hand-strength "KAAAK" false) 5))
(assert (= (hand-strength "AKAJA" false) 4))
(assert (= (hand-strength "AKATK" false) 3))
(assert (= (hand-strength "A234A" false) 2))
(assert (= (hand-strength "QKJTA" false) 1))

(defn ->player
  [hand bid wild?]
  {:hand hand
   :strength (hand-strength hand wild?)
   :bet bid})

(def suit-strengths
  {\2 1
   \3 2
   \4 3
   \5 4
   \6 5
   \7 6
   \8 7
   \9 8
   \T 9
   \J 10
   \Q 11
   \K 12
   \A 13})
(def wild-suit-strengths
  {\2 1
   \3 2
   \4 3
   \5 4
   \6 5
   \7 6
   \8 7
   \9 8
   \T 9
   \J 0
   \Q 11
   \K 12
   \A 13})

(defn compare-player
  [p1 p2 wild?]
  (let [strength-compare (compare (:strength p1) (:strength p2))]
    (if (zero? strength-compare)
      (loop [[card1 & hand1] (:hand p1)
             [card2 & hand2] (:hand p2)]
        (let [card-compare (if wild?
                             (compare (wild-suit-strengths card1) (wild-suit-strengths card2))
                             (compare (suit-strengths card1) (suit-strengths card2)))]
          (if (zero? card-compare)
            (recur hand1 hand2)
            card-compare)))
      strength-compare)))

(defn parse-player
  [s wild?]
  (let [[_ hand bid] (re-find #"^([2-9TJQKA]{5,5}) +(\d+)$" s)]
    (->player hand (parse-long bid) wild?)))

(defn parse-players
  [wild? s]
  (->> s
       (str/split-lines)
       (mapv #(parse-player % wild?))))

(defn part1
  [filename]
  (->> (slurp filename)
       (parse-players false)
       (sort #(compare-player %1 %2 false))
       (map-indexed (fn [idx player] (* (inc idx) (:bet player))))
       (apply +)))

(assert (= 6440 (part1 "day07.example")))
(assert (= 250602641 (time (part1 "day07.input"))))


(defn part2
  [filename]
  (->> (slurp filename)
       (parse-players true)
       (sort #(compare-player %1 %2 true))
       (map-indexed (fn [idx player] (* (inc idx) (:bet player))))
       (apply +)))

(assert (= 5905 (part2 "day07.example")))
(assert (= 251037509 (time (part2 "day07.input"))))
