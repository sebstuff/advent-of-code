(ns day09
  (:require [clojure.string :as str]))

(defn parse-motion
  "parses a motion from the string"
  [s]
  (let [[dir steps] (re-seq #"[^ ]+" s)]
    [({"R" :right "L" :left "U" :up "D" :down} dir)
     (parse-long steps)]))

(defn load-motions
  "loads the motions"
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map parse-motion)))
#_(load-motions "day09.example")

(defn move
  "moves a position the direction"
  [[x y] dir]
  (case dir
    :up [x (dec y)]
    :down [x (inc y)]
    :left [(dec x) y]
    :right [(inc x) y]))

(defn neighbors
  "returns all positions neighboring the given one"
  [[x y]]
  (for [dx (range -1 2)
        dy (range -1 2)
        :let [x' (- x dx)
              y' (- y dy)]
        :when (or (not= x' x)
                  (not= y' y))]
    [x' y']))
#_(neighbors [0 0])


(defn dec-magnitude
  "Decrements the magnitude of the value.
  For example:
  -1 => 0
  -2 => -1
   2 => 1
   1 => 0"
  [val]
  (cond
    (= 0 val) val
    (pos? val) (dec val)
    :else (inc val)))
(assert (= 0 (dec-magnitude 0)))
(assert (= -2 (dec-magnitude -3)))
(assert (= 2 (dec-magnitude 3)))

(defn follow
  "causes follower to follow the leader"
  [leader follower]
  (if (or (= leader follower)
          (some (partial = follower) (neighbors leader)))
    follower
    (let [[lx ly] leader
          [fx fy] follower
          dx (- lx fx)
          dy (- ly fy)]
      (cond
        (= 0 dx) [fx (+ fy (dec-magnitude dy))]
        (= 0 dy) [(+ fx (dec-magnitude dx)) fy]
        ;; need to move diagonally
        :else
        [(+ fx (/ dx (abs dx)))
         (+ fy (/ dy (abs dy)))]))))
(assert (= [1 1] (follow [1 1] [1 1])))
(assert (= [1 2] (follow [1 1] [1 2])))
(assert (= [1 2] (follow [1 1] [1 3])))
(assert (= [2 2] (follow [1 1] [3 3])))
(assert (= [4 -1] (follow [4 -2] [3 0])))


(defn step
  [[head-coords & rest] dir]
  (loop [sofar [(conj head-coords (move (last head-coords) dir))]
         rest rest]
    (if (seq rest)
      (let [prev-coords (last sofar)
            prev (last prev-coords)
            [cur-coords & rest'] rest
            cur' (follow prev (last cur-coords))
            cur-coords' (conj cur-coords cur')]
        (recur (conj sofar cur-coords') rest'))
      sofar)
    ))

(defn make-motion
  [state [dir steps]]
  (reduce step state (repeat steps dir)))

(defn play-motions
  [motions knots]
  (reduce make-motion
          (vec (repeat knots [[0 0]]))
          motions))

(defn path->str
  [path]
  (let [path (frequencies path)
        top-x (apply max (map ffirst path))
        bottom-x (apply min (map ffirst path))
        top-y (apply max (map (comp second first) path))
        bottom-y (apply min (map (comp second first) path))]
    (str/join
     (for [y (range bottom-y (inc top-y))]
       (str/join
        (conj (for [x (range bottom-x (inc top-x))]
                (if-let [n (get path [x y])]
                  (if (< n 10) (str n) "*")
                  \.))
              \newline))))))

(defn knots->str
  [knots top-x bottom-x top-y bottom-y]
  (let [coords (zipmap (reverse knots)
                       (range (dec (count knots)) -1 -1))]
    (str/join
     (for [y (range bottom-y (inc top-y))]
       (str/join
        (conj (for [x (range bottom-x (inc top-x))]
                (if-let [n (get coords [x y])]
                  (str (if (< n 10) n (char (+ (int \A) (- n 10)))))
                  \.))
              \newline))))))

(defn solve
  [filename knots]
  (-> (load-motions filename)
      (play-motions knots)
      (get (dec knots))
      (set)
      (count)))

(defn render-solve
  [filename num-knots]
  (let [motions (load-motions filename)
        knots-paths (play-motions motions num-knots)
        xs (mapcat #(map first %) knots-paths)
        ys (mapcat #(map second %) knots-paths)
        top-x (apply max xs)
        bottom-x (apply min xs)
        top-y (apply max ys)
        bottom-y (apply min ys)]
    (loop [frame 0]
      (println (knots->str (map #(nth % frame) knots-paths)
                           top-x bottom-x top-y bottom-y))
      (Thread/sleep 50)
      (when (< frame (dec (count (first knots-paths))))
        (recur (inc frame))))))

(defn part1
  [filename]
  (solve filename 2))

(assert (= 13 (part1 "day09.example")))
(assert (= 6376 (part1 "day09.input")))


(defn part2
  [filename]
  (solve filename 10))

(assert (= 36 (part2 "day09.example2")))
(assert (= 2607 (part2 "day09.input")))

(render-solve "day09.example2" 10)
