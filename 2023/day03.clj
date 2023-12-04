(ns day03
  (:require [clojure.string :as str]))

(defn parse-line
  [schematic line]
  (let [y (:top-y schematic)]
    (loop [schematic schematic
           [[_ token] & tokens] (re-seq #"(\d+|\.|[^\d\.])" line)
           x 0]
      (if-not token
        (update schematic :top-y inc)
        (cond
          (= "." token)
          (recur schematic tokens (inc x))

          (Character/isDigit (first token))
          (let [x' (+ x (count token))
                part {:number (parse-long token) :x x :y y :width (count token)}]
            (recur (update schematic :parts conj part) tokens x'))

          :else
          (let [symbol {:x x :y y :ch token}]
            (recur (update schematic :symbols conj symbol) tokens (inc x))))))))

(defn parse-input
  [s]
  (let [schematic {:parts [] :symbols [] :top-y 0}]
    (->> s
        (str/split-lines)
        (reduce parse-line schematic))))

(defn symbol-adjacent-to-part?
  [symbol {:keys [x y width]}]
  (let [left-x (dec x)
        right-x (+ x width)
        top-y (dec y)
        bottom-y (+ y 1)]
    (let [sx (:x symbol)
          sy (:y symbol)]
      (and (<= left-x sx)
           (>= right-x sx)
           (<= top-y sy)
           (>= bottom-y sy)))))

(defn remove-invalid-parts
  [schematic]
  (update schematic :parts (fn [parts]
                             (filter (fn part-adjacent-to-any-symbol?[part]
                                       (some #(symbol-adjacent-to-part? % part)
                                             (:symbols schematic)))
                                     parts))))

(defn part1
  [filename]
  (->> (slurp filename)
      (parse-input)
      (remove-invalid-parts)
      (:parts)
      (map :number)
      (apply +)))

(assert (= 4361 (part1 "day03.example")))
(assert (= 527364 (part1 "day03.input")))

(println "Part1:" (part1 "day03.input"))

(defn symbols->gears
  [schematic]
  (let [{:keys [symbols gears]} (group-by (fn [{:keys [ch] :as symbol}]
                                            (if (not= ch "*")
                                              :symbols
                                              (let [adjacent-parts (filter #(symbol-adjacent-to-part? symbol %)
                                                                           (:parts schematic))]
                                                (if (= 2 (count adjacent-parts))
                                                  :gears
                                                  :symbols))))
                                          (:symbols schematic))]
    (assoc schematic :symbols symbols :gears gears)))

(defn gear-ratio
  [gear schematic]
  (->> (:parts schematic)
       (filter #(symbol-adjacent-to-part? gear %))
       (map :number)
       (apply *)))

(defn part2
  [filename]
  (let [schematic (->> (slurp filename)
                       (parse-input)
                       (symbols->gears))]
    (->> (:gears schematic)
         (map #(gear-ratio % schematic))
         (apply +))))

(assert (= 467835 (part2 "day03.example")))
(assert (= 79026871 (part2 "day03.input")))
(println "Part2:" (part2 "day03.input"))
