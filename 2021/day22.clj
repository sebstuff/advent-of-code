(ns day22
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn load-data
  [filename]
  (->> filename
       (slurp)
       (str/split-lines)
       (map #(rest (re-matches #"^(.+) x=(.+)\.\.(.+),y=(.+)\.\.(.+),z=(.+)\.\.(.+)$" %)))
       (map (fn [[cmd & coords]]
              {:cmd cmd
               :coords (->> coords
                            (map #(Integer/parseInt %))
                            (partition 2 2))}))))

(defn cuboid-cubes
  [{[[min-x max-x] [min-y max-y] [min-z max-z]] :coords}]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))
        z (range min-z (inc max-z))]
    [x, y, z]))


(defn clamp-coord
  [[low high]]
  [(min 51 (max -50 low))
   (min 50 (max -51 high))])

(defn clamp-cuboid
  [{:keys [coords] :as cuboid}]
  (assoc cuboid :coords (map clamp-coord coords)))

(defn activate-cuboid
  [coords-on {:keys [cmd] :as cuboid}]
  (case cmd
    "on" (apply conj coords-on (cuboid-cubes cuboid))
    "off" (apply disj coords-on (cuboid-cubes cuboid))))

(defn part1
  [cuboids]
  (->> cuboids
       (map clamp-cuboid)
       (reduce activate-cuboid #{})
       (count)))

(println "part1:" (part1 (load-data "day22.input")))
