(ns day20
  (:require [clojure.string :as string]))

(defn pixel-value
  [image pixel]
  (get image pixel (:default image)))

(defn parse-input
  [input]
  (let [lines (string/split-lines input)
        algorithm (first lines)
        pixels (into {:default \.}
                     (mapcat (fn [rownum row]
                               (map (fn [colnum pixel]
                                      [[colnum rownum] pixel])
                                    (range)
                                    row))
                             (range)
                             (rest (rest lines))))]
    {:algorithm algorithm
     :image pixels}))

(defn pixel-region
  "Gets the region used for enhancement for the pixel."
  [[x y] image]
  (for [dy [-1 0 1]
        dx [-1 0 1]
        :let [x' (+ x dx)
              y' (+ y dy)]]
    (pixel-value image [x' y'])))

(defn pixel-region->algorithm-idx
  "Converts a pixel region into the index into the algorithm array."
  [region]
  (let [region-with-idx (map list (reverse region) (range))]
    (reduce (fn [sofar [c idx]]
              (if (= c \#)
                (bit-set sofar idx)
                sofar))
            0
            region-with-idx)))

(defn enhanced-pixel-value
  "Gets the enhanced value for a pixel."
  [pixel image algorithm]
  (let [region (pixel-region pixel image)
        idx (pixel-region->algorithm-idx region)]
    (nth algorithm idx)))

(defn enhance
  [{:keys [algorithm image]}]
  (let [pixels (filter #(not= :default %) (keys image))
        min-x (- (apply min (map first pixels)) 1)
        max-x (+ (apply max (map first pixels)) 2)
        min-y (- (apply min (map second pixels)) 1)
        max-y (+ (apply max (map second pixels)) 2)
        pixel-values' (for [x (range min-x (inc max-x))
                           y (range min-y (inc max-y))
                           :let [pixel [x y]]]
                       [pixel (enhanced-pixel-value pixel image algorithm)])
        default-value (enhanced-pixel-value [(dec min-x) (dec min-y)] image algorithm)
        image' (into {:default default-value} pixel-values')]
    {:algorithm algorithm
     :image image'}))

(defn count-pixel-values
  [image value]
  (if (= (:default image) value)
    :infinity
    (count (filter (fn [[pixel val]] (= val value)) (dissoc image :default)))))

(defn part1
  [filename]
  (let [input (->> filename
                   slurp
                   parse-input)
        output (-> input
                   enhance
                   enhance)]
    (count-pixel-values (:image output) \#)))

(assert (= 35 (part1 "day20.example")))
(println "part1" (part1 "day20.input"))

(defn part2
  [filename]
  (let [input (->> filename
                   slurp
                   parse-input)
        output (last (take 51 (iterate enhance input)))]
    (count-pixel-values (:image output) \#)))

(assert (= 3351 (part2 "day20.example")))
(println "part2" (part2 "day20.input"))
