(ns day08
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn width
  [forest]
  (count (first forest)))

(defn height
  [forest]
  (count forest))

(defn load-data
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (mapv #(mapv (comp parse-long str) %))))

(defn visible-from-coord-list
  "Returns trees visible when looking across the list of coords"
  [forest coord-list]
  (loop [visible #{}
         [[x y] & rest-coords] coord-list
         top-tree -1]
    (let [tree (get-in forest [y x])
          visible? (> tree top-tree)
          visible' (if visible? (conj visible [x y]) visible)]
      (if (seq rest-coords)
        (recur visible' rest-coords (max top-tree tree))
        visible'))))

(defn visible-from-coord-lists
  "Kinda like visible-from-coord-list, but its a list of a list of coords"
  [forest coord-lists]
  (reduce #(set/union %1 (visible-from-coord-list forest %2))
          #{}
          coord-lists))

(defn visible-from-left
  [forest]
  (visible-from-coord-lists
   forest
   (for [y (range (height forest))]
     (for [x (range (width forest))]
       [x y]))))

(defn visible-from-right
  [forest]
  (visible-from-coord-lists
   forest
  (for [y (range (height forest))]
     (for [x (range (dec (width forest)) 0 -1)]
       [x y]))))

(defn visible-from-top
  [forest]
  (visible-from-coord-lists
   forest
   (for [x (range (width forest))]
     (for [y (range (height forest))]
       [x y]))))

(defn visible-from-bottom
  [forest]
  (visible-from-coord-lists
   forest
   (for [x (range (width forest))]
     (for [y (range (dec (height forest)) 0 -1)]
       [x y]))))

(defn part1
  [filename]
  (let [data (load-data filename)]
    (count (set/union
            (visible-from-left data)
            (visible-from-right data)
            (visible-from-top data)
            (visible-from-bottom data)))))

(assert (= 21 (part1 "day08.example")))
(assert (= 1809 (part1 "day08.input")))


(defn scenic-score
  [forest x y]
  (let [up-score (loop [score 0
                        y' (dec y)]
                   (cond
                     (= -1 y')
                     score

                     (>= (get-in forest [y' x]) (get-in forest [y x]))
                     (inc score)

                     :else
                     (recur (inc score) (dec y'))))
        down-score (loop [score 0
                        y' (inc y)]
                   (cond
                     (= (height forest) y')
                     score

                     (>= (get-in forest [y' x]) (get-in forest [y x]))
                     (inc score)

                     :else
                     (recur (inc score) (inc y'))))
        left-score (loop [score 0
                        x' (dec x)]
                   (cond
                     (= -1 x')
                     score

                     (>= (get-in forest [y x']) (get-in forest [y x]))
                     (inc score)

                     :else
                     (recur (inc score) (dec x'))))
        right-score (loop [score 0
                           x' (inc x)]
                      (cond
                        (= (width forest) x')
                        score

                        (>= (get-in forest [y x']) (get-in forest [y x]))
                        (inc score)

                        :else
                        (recur (inc score) (inc x'))))]
    (* up-score left-score down-score right-score)))

(defn part2
  [filename]
  (let [data (load-data filename)]
    (apply max
           (for [y (range (width data))
                 x (range (height data))]
             (scenic-score data x y)))))

(assert (= 8 (part2 "day08.example")))
(assert (= 479400 (part2 "day08.input")))
