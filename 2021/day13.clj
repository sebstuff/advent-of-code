(ns day13
  (:require [clojure.string :as string]))


(defn parse-input
  [input]
  (let [[dots instructions] (->> input
                                 string/split-lines
                                 (filter #(not= "" %))
                                 (partition-by #(= 0 (string/index-of % "fold"))))
        dots (map (fn str->dots[dot]
                    (let [[x y] (string/split dot #",")
                          x (Integer/parseInt x)
                          y (Integer/parseInt y)]
                      [x y])) dots)
        instructions (map (fn str->instructions[instruction]
                            (let [instruction (string/replace instruction "fold along " "")
                                  [dimension coord] (string/split instruction #"=")
                                  instruction [(keyword dimension) (Integer/parseInt coord)]]
                              instruction)) instructions)]
    {:dots dots :instructions instructions}))
(defn fold-up
  [dots coord]
  (let [[keep-dots folding-dots] (->> dots
                                      (group-by #(< coord (second %)))
                                      (#(conj [] (get % false) (get % true))))
        folded-dots (map (fn fold-dots[[x y]]
                           [x (- (* 2 coord) y)]) folding-dots)]
    (into #{} (concat keep-dots folded-dots))))


(defn fold-left
  [dots coord]
  (let [[keep-dots folding-dots] (->> dots
                                      (group-by #(< coord (first %)))
                                      (#(conj [] (get % false) (get % true))))
        folded-dots (map (fn fold-dots[[x y]]
                           [(- (* 2 coord) x) y]) folding-dots)]
    (into #{} (concat keep-dots folded-dots))))

(defn fold
  [dots instruction]
  (case (first instruction)
    :y (fold-up dots (second instruction))
    :x (fold-left dots (second instruction))))

(defn part1
  [filename]
  (let [{:keys [dots instructions]} (->> filename
                                         slurp
                                         parse-input)]
    (-> dots
        (fold (first instructions))
        count)))

(assert (= 17 (part1 "day13.example")))
(println "part1" (part1 "day13.input"))

(defn dots->grid
  [dots]
  (let [dots (into #{} dots)
        max-x (apply max (map first dots))
        max-y (apply max (map second dots))]
    (for [y (range (inc max-y))]
      (for [x (range (inc max-x))]
        (if (dots [x y])
          \#
          \.)))))

(defn part2
  [filename]
  (let [{:keys [dots instructions]} (->> filename
                                         slurp
                                         parse-input)]
    (dots->grid (reduce #(fold %1 %2) dots instructions))))

(println "part2")
(dorun (map println (part2 "day13.input")))
