(ns day21
  (:require [clojure.string :as str]
            [clojure.zip :as zip]))

(defn parse-players
  [input]
  (->> input
       (str/split-lines)
       (map (fn [line]
              (let [[_ start-position] (re-matches #"Player . starting position: (.+)" line)]
                (Integer/parseInt start-position))))
       (map #(assoc {:score 0} :space (dec %)))))


(def rng (atom 0))

(defn init-rand
  []
  (reset! rng 0))

(defn next-roll
  []
  (+ (swap! rng inc)
     (swap! rng inc)
     (swap! rng inc)))

(defn wins?
  [player]
  (<= 1000 (:score player)))

(defn turn
  [player]
  (let [roll (next-roll)
        space (-> (:space player)
                  (+ roll)
                  (mod 10))
        score (+ (inc space) (:score player))]
    {:score score :space space}))

(defn round
  [players]
  (loop [z (zip/next (zip/vector-zip (vec players)))]
    (if (zip/end? z)
      (zip/root z)
      (let [z' (zip/edit z turn)]
        (if (wins? (zip/node z'))
          (zip/root z')
          (recur (zip/next z')))))))

(defn play
  [players]
  (init-rand)
  (loop [players players]
    (if (some wins? players)
      players
      (recur (round players)))))

(defn part1
  [players]
  (let [players' (play players)
        min-score (apply min (map :score players'))]
    (* min-score @rng)))

(def example "Player 1 starting position: 4\nPlayer 2 starting position: 8")
(def my-input "Player 1 starting position: 6\nPlayer 2 starting position: 9")

(println "part1:" (part1 (parse-players my-input)))
