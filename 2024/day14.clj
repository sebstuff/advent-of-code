(ns day14
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))


(defn parse-robot
  [s]
  (let [[_ px py vx vy] (re-matches #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)"s)]
    {:position [(parse-long px) (parse-long py)]
     :velocity [(parse-long vx) (parse-long vy)]}))

(defn parse-robots
  [s]
  (->> (str/split-lines s)
       (mapv parse-robot)))

(defn simulate
  [robots times dimensions]
  (mapv (fn [robot]
          (let [{:keys [position velocity]} robot
                position' (mapv (fn [p v d]
                                  (let [r (rem (+ p (* v times))
                                               d)]
                                    (if (> 0 r)
                                      (+ d r)
                                      r)))
                                position
                                velocity
                                dimensions)]
            (assoc robot :position position')))
        robots))

(defn assign-quadrants
  [robots [mx my :as dimensions]]
  (reduce (fn [sofar robot]
            (let [[x y] (:position robot)]
              (cond
                ;; top left
                (and (< -1 x (quot mx 2))
                     (< -1 y (quot my 2)))
                (update sofar :top-left conj (:position robot))

                ;; top right
                (and (< (quot mx 2) x mx)
                     (< -1 y (quot my 2)))
                (update sofar :top-right conj (:position robot))

                ;; bottom left
                (and (< -1 x (quot mx 2))
                     (< (quot my 2) y my))
                (update sofar :bottom-left conj (:position robot))

                ;; bottom right
                (and (< (quot mx 2) x mx)
                     (< (quot my 2) y my))
                (update sofar :bottom-right conj (:position robot))

                ;; center
                :else
                (update sofar :center conj (:position robot)))))
          {}
          robots))

(defn part1
  [filename times dimensions]
  (let [robots (->> (slurp filename)
                    (parse-robots))
        robots' (simulate robots times dimensions)
        quadrant-robots (assign-quadrants robots' dimensions)]
    (->> quadrant-robots
         (filter (fn [[k v]] (not= k :center)))
         (map second)
         (map count)
         (apply *))))

(assert (= 12 (part1 "day14.example" 100 [11 7])))
(pp/pprint (time (part1 "day14.input" 100 [101 103])))
