(ns day25
  (:require [clojure.string :as string]))

(defn load-data
  [filename]
  (->> (slurp filename)
       (string/split-lines)
       (mapv (comp vec char-array))))

(defn at
  [m x y]
  ((m y) x))

(defn put
  [m x y ch]
  (assoc-in m [y x] ch))

(defn height
  [m]
  (count m))

(defn width
  [m]
  (count (first m)))

(defn next-step
  [x y m]
  (case (at m x y)
    \> [(mod (inc x) (width m)) y]
    \v [x (mod (inc y) (height m))]))
(assert (= [1 0] (next-step 0 0 [[\> \. \.] [\. \. \.]]))) ; test east
(assert (= [0 1] (next-step 0 0 [[\v \. \.] [\. \. \.]]))) ; test south
(assert (= [0 0] (next-step 2 0 [[\. \. \>] [\. \. \.]]))) ; test east wrapping
(assert (= [2 0] (next-step 2 1 [[\. \. \.] [\. \. \v]]))) ; test south wrapping

(defn move
  [m x y x' y']
  (let [ch (at m x y)]
    (-> m
        (put x y \.)
        (put x' y' ch))))

(defn place-step
  [x y m m-sofar]
  (let [[x' y'] (next-step x y m)]
    (if (= \. (at m x' y'))
      (move m-sofar x y x' y')
      m-sofar)))
(assert (= [[\. \> \.] [\. \. \.]] (place-step 0 0 [[\> \. \.] [\. \. \.]] [[\> \. \.] [\. \. \.]]))) ; move east
(assert (= [[\. \. \.] [\. \v \.]] (place-step 1 0 [[\. \v \.] [\. \. \.]] [[\. \v \.] [\. \. \.]]))) ; move south
(assert (= [[\> \> \.] [\. \. \.]] (place-step 0 0 [[\> \> \.] [\. \. \.]] [[\> \> \.] [\. \. \.]]))) ; move blocked

(defn pieces-step
  [m p]
  (reduce (fn [m-sofar [x y]] (place-step x y m m-sofar))
          m
          (for [x (range (width m))
                y (range (height m))
                :when (= p (at m x y))]
            [x y])))

(defn step
  [m]
  (pieces-step (pieces-step m \>) \v))

(defn step-stops-moving
  [m c]
  (let [m' (step m)]
    (if (= m m')
      (inc c)
      (recur m' (inc c)))))

(defn part1
  [filename]
  (step-stops-moving (load-data filename) 0))

(assert (= 58 (part1 "day25.example")))
(assert (= 523 (part1 "day25.input")))
