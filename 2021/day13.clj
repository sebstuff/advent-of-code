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
                      {:x x :y y})) dots)
        instructions (map (fn str->instructions[instruction]
                            (let [instruction (string/replace instruction "fold along " "")
                                  [axis coord] (string/split instruction #"=")
                                  instruction [(keyword axis) (Integer/parseInt coord)]]
                              instruction)) instructions)]
    {:dots dots :instructions instructions}))

(defn fold-paper
  [dots instruction]
  (let [[axis coord] instruction
        [keep-dots folding-dots] (->> dots
                                      (group-by #(< coord (get % axis)))
                                      (#(conj [] (get % false) (get % true))))
        folded-dots (map (fn fold-dots[dot]
                           (update dot axis #(- (* 2 coord) %))) folding-dots)]
    (into #{} (concat keep-dots folded-dots))))

(defn part1
  [filename]
  (let [{:keys [dots instructions]} (->> filename
                                         slurp
                                         parse-input)]
    (-> dots
        (fold-paper (first instructions))
        count)))

(assert (= 17 (part1 "day13.example")))
(println "part1" (part1 "day13.input"))

(defn dots->grid
  [dots]
  (let [dots (into #{} dots)
        max-x (apply max (map :x dots))
        max-y (apply max (map :y dots))]
    (for [y (range (inc max-y))]
      (for [x (range (inc max-x))]
        (if (dots {:x x :y y})
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
