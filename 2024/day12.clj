(ns day12
  (:require [clojure.string :as str]))

(defn parse-input
  [s]
  (->> (str/split-lines s)
       (map-indexed (fn [row line]
                      (map-indexed (fn [col ch]
                                     [[row col] ch])
                                   line)))
       (mapcat identity)
       (into {})))


(defn neighbors
  [[row col :as coord]]
  [[(dec row) col]
   [(inc row) col]
   [row (dec col)]
   [row (inc col)]])


(defn regions
  [plant-map]
  (loop [[coord & coords] (keys plant-map)
         next-region 0
         coord-region-map {}]
    (cond
      (nil? coord)
      coord-region-map
      
      (get coord-region-map coord)
      (recur coords next-region coord-region-map)

      :else
      (let [existing-region (loop [[visit-coord & visit-coords] [coord]
                                       visited-coords #{coord}]
                                  (cond
                                    (nil? visit-coord)
                                    nil

                                    (get coord-region-map visit-coord)
                                    (get coord-region-map visit-coord)

                                    :else
                                    (let [new-coords-raw (neighbors visit-coord)
                                          new-coords (->> new-coords-raw
                                                          (filter plant-map)
                                                          (filter #(= (get plant-map coord) (get plant-map %)))
                                                          (filter #(nil? (get visited-coords %))))]
                                      (recur (concat visit-coords new-coords)
                                             (apply conj visited-coords new-coords)))))]
        (if existing-region
          (recur coords
                 next-region
                 (assoc coord-region-map coord existing-region))
          (recur coords
                 (inc next-region)
                 (assoc coord-region-map coord next-region)))))))

(defn fence-counts
  [plant-map]
  (into {} (map (fn [[coord plant]]
                  [coord
                   (count (filter #(not= (get plant-map %) (get plant-map coord))
                                  (neighbors coord)))])
                plant-map)))

(defn region-areas
  [region-coords]
  (into {} (map (fn [[k v]] [k (count v)])
                region-coords)))

(defn region-coords
  [region-map]
  (reduce (fn [sofar [k v]]
            (if (sofar v)
              (update sofar v conj k)
              (assoc sofar v #{k})))
          {}
          region-map))

(defn region-fence-counts
  [region-coords fence-count-map]
  (into {} (map (fn [[region coords]]
                  [region
                   (apply + (map fence-count-map coords))])
                region-coords)))

(defn region-prices
  [region-areas region-fence-counts]
  (into {} (map (fn [region]
                  [region (* (region-fence-counts region) (region-areas region))])
                (keys region-areas))))

(defn part1
  [filename]
  (let [plant-map (parse-input (slurp filename))
        region-map (regions plant-map)
        region-coords (region-coords region-map) 
        region-areas (region-areas region-coords) 
        fence-count-map (fence-counts plant-map)
        region-fence-counts (region-fence-counts region-coords fence-count-map)
        region-prices (region-prices region-areas region-fence-counts)]
    (apply + (vals region-prices))))

(println (time (part1 "day12.input")))
