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

(defn visible-across-coord-list
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

(defn visible-across-coord-lists
  "Kinda like visible-across-coord-list, but its a list of a list of coords"
  [forest coord-lists]
  (reduce #(set/union %1 (visible-across-coord-list forest %2))
          #{}
          coord-lists))

(defn visible-from-left
  [forest]
  (visible-across-coord-lists
   forest
   (for [y (range (height forest))]
     (for [x (range (width forest))]
       [x y]))))

(defn visible-from-right
  [forest]
  (visible-across-coord-lists
   forest
  (for [y (range (height forest))]
     (for [x (range (dec (width forest)) 0 -1)]
       [x y]))))

(defn visible-from-top
  [forest]
  (visible-across-coord-lists
   forest
   (for [x (range (width forest))]
     (for [y (range (height forest))]
       [x y]))))

(defn visible-from-bottom
  [forest]
  (visible-across-coord-lists
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

(defn scenic-score-using-coord-list
  "Calculates the scenic score from a tree when looking out across a list of coords."
  [forest x y coords]
  (let [coords-lower (take-while
                      #(> (get-in forest [y x])
                          (get-in forest [(second %) (first %)]))
                      coords)]
    (min (inc (count coords-lower))
         (count coords))))

(defn scenic-score
  [forest x y]
  (let [up-score (scenic-score-using-coord-list
                  forest x y
                  (for [y' (range (dec y) -1 -1)]
                    [x y']))
        down-score (scenic-score-using-coord-list
                    forest x y
                    (for [y' (range (inc y) (height forest))]
                      [x y']))
        left-score (scenic-score-using-coord-list
                    forest x y
                    (for [x' (range (dec x) -1 -1)]
                      [x' y]))
        right-score (scenic-score-using-coord-list
                     forest x y
                     (for [x' (range (inc x) (width forest))]
                       [x' y]))]
    (* up-score down-score left-score right-score)))

(defn part2
  [filename]
  (let [data (load-data filename)]
    (apply max
           (for [y (range (width data))
                 x (range (height data))]
             (scenic-score data x y)))))

(assert (= 8 (part2 "day08.example")))
(assert (= 479400 (part2 "day08.input")))
