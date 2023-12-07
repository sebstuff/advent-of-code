(ns day07
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(def ^:dynamic *hand-strength* nil)
(def ^:dynamic *suit-strengths* nil)

(defn hand-strength
  [hand]
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
      1)))
(assert (= (hand-strength "AAAAA") 7))
(assert (= (hand-strength "KAAAA") 6))
(assert (= (hand-strength "AAKAA") 6))
(assert (= (hand-strength "AAAAK") 6))
(assert (= (hand-strength "AAAAK") 6))
(assert (= (hand-strength "AAAKK") 5))
(assert (= (hand-strength "AKAKA") 5))
(assert (= (hand-strength "KAAAK") 5))
(assert (= (hand-strength "AKAJA") 4))
(assert (= (hand-strength "AKATK") 3))
(assert (= (hand-strength "A234A") 2))
(assert (= (hand-strength "QKJTA") 1))

(defn ->player
  [hand bid]
  {:hand hand
   :strength (*hand-strength* hand)
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

(defn compare-player
  [p1 p2]
  (let [strength-compare (compare (:strength p1) (:strength p2))]
    (if (zero? strength-compare)
      (loop [[card1 & hand1] (:hand p1)
             [card2 & hand2] (:hand p2)]
        (let [card-compare (compare (*suit-strengths* card1) (*suit-strengths* card2))]
          (if (zero? card-compare)
            (recur hand1 hand2)
            card-compare)))
      strength-compare)))

(defn parse-player
  [s]
  (let [[_ hand bid] (re-find #"^([2-9TJQKA]{5,5}) +(\d+)$" s)]
    (->player hand (parse-long bid))))

(defn parse-players
  [s]
  (->> s
       (str/split-lines)
       (mapv parse-player)))

(defn part1
  [filename]
  (binding [*hand-strength* hand-strength
            *suit-strengths* suit-strengths]
    (->> (slurp filename)
         (parse-players)
         (sort compare-player)
         (map-indexed (fn [idx player] (* (inc idx) (:bet player))))
         (apply +))))

(assert (= 6440 (part1 "day07.example")))
(assert (= 250602641 (time (part1 "day07.input"))))

(defn wild-hand-strength
  [hand]
  (let [wildcards (count (filter #(= \J %) hand))
        card-matches (->> hand
                          (remove #(= \J %))
                          (frequencies)
                          (map reverse)
                          (sort-by first >)
                          (vec))
        card-matches (case wildcards
                       0 card-matches
                       5 [[wildcards \J]]
                       (update card-matches 0 (fn apply-wildcards[[count card]]
                                                [(+ count wildcards) card])))
        new-hand (apply concat (map #(repeat (first %) (second %))
                                    card-matches))]
    (hand-strength new-hand)))

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

(defn part2
  [filename]
  (binding [*hand-strength* wild-hand-strength
            *suit-strengths* wild-suit-strengths]
    (->> (slurp filename)
         (parse-players)
         (sort compare-player)
         (map-indexed (fn [idx player] (* (inc idx) (:bet player))))
         (apply +))))

(assert (= 5905 (part2 "day07.example")))
(assert (= 251037509 (time (part2 "day07.input"))))
