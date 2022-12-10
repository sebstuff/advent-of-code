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

(defn visible-from-left
  [forest]
  (loop [visible #{}
         x 0
         y 0
         top-tree -1]
    (let [tree (get-in forest [y x])
          visible? (> tree top-tree)
          visible' (if visible? (conj visible [x y]) visible)]
      (cond
        (and (= (inc x) (width forest))
             (= (inc y) (height forest)))
        visible'

        (= (inc x) (width forest))
        (recur visible' 0 (inc y) -1)

        :else
        (recur visible' (inc x) y (max top-tree tree))))))

(defn visible-from-right
  [forest]
  (loop [visible #{}
         x (dec (width forest))
         y 0
         top-tree -1]
    (let [tree (get-in forest [y x])
          visible? (> tree top-tree)
          visible' (if visible? (conj visible [x y]) visible)]
      (cond
        (and (= 0 x)
             (= (inc y) (height forest)))
        visible'

        (= x 0)
        (recur visible' (dec (width forest)) (inc y) -1)

        :else
        (recur visible' (dec x) y (max top-tree tree))))))

(defn visible-from-top
  [forest]
  (loop [visible #{}
         x 0
         y 0
         top-tree -1]
    (let [tree (get-in forest [y x])
          visible? (> tree top-tree)
          visible' (if visible? (conj visible [x y]) visible)]
      (cond
        (and (= (inc x) (width forest))
             (= (inc y) (height forest)))
        visible'

        (= (inc y) (height forest))
        (recur visible' (inc x) 0 -1)

        :else
        (recur visible' x (inc y) (max top-tree tree))))))

(defn visible-from-bottom
  [forest]
  (loop [visible #{}
         x 0
         y (dec (height forest))
         top-tree -1]
    (let [tree (get-in forest [y x])
          visible? (> tree top-tree)
          visible' (if visible? (conj visible [x y]) visible)]
      (cond
        (and (= (inc x) (width forest))
             (= y 0))
        visible'

        (= 0 y)
        (recur visible' (inc x) (dec (height forest)) -1)

        :else
        (recur visible' x (dec y) (max top-tree tree))))))

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
