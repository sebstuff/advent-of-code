(ns day02
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def play-score
  {:rock 1
   :paper 2
   :scissors 3})
(def result-score
  {:lose 0
   :draw 3
   :win 6})
(def loses-against-play
  {:rock :paper
   :paper :scissors
   :scissors :rock})
(def wins-against-play
  (set/map-invert loses-against-play))

(defn round-result
  [opponent me]
  (cond
    (= me opponent) :draw
    (= me (get loses-against-play opponent)) :win
    :else :lose))

(assert (= :lose (round-result :rock :scissors)))
(assert (= :win (round-result :rock :paper)))
(assert (= :draw (round-result :rock :rock)))

(defn score-round
  [opponent me]
  (+ (get play-score me)
     (get result-score (round-result opponent me))))

(defn opponent-letter->play
  [l]
  (case l
    \A :rock
    \B :paper
    \C :scissors))

(defn my-letter->play
  [l]
  (case l
    \X :rock
    \Y :paper
    \Z :scissors))

(defn ^:dynamic parse-round
  [str]
  [(opponent-letter->play (nth str 0))
   (my-letter->play (nth str 2))])

(defn load-rounds
  [filename]
  (->> (slurp filename)
       (string/split-lines)
       (map parse-round)))

(defn part1
  [filename]
  (->> filename
       (load-rounds)
       (map #(apply score-round %))
       (reduce + 0)))

(assert (= 15 (part1 "day02.example")))
(assert (= 12855 (part1 "day02.input")))


(defn my-letter->result
  [l]
  (case l
    \X :lose
    \Y :draw
    \Z :win))

(defn play-for-result
  [opponent result]
  (case result
    :win (get loses-against-play opponent)
    :lose (get wins-against-play opponent)
    :draw opponent))

(defn parse-round-part2
  [str]
  (let [opponent (opponent-letter->play (nth str 0))
        desired-result (my-letter->result (nth str 2))
        me (play-for-result opponent desired-result)]
    [opponent me]))

(binding [parse-round parse-round-part2]
  (assert (= 12 (part1 "day02.example")))
  (assert (= 13726 (part1 "day02.input"))))
