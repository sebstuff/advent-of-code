(ns day05
  (:require [clojure.string :as str]))

(defn parse-crate-slice
  [stacks crate-slice-data]
  (loop [stacks stacks
         idx 0]
    (let [pos (inc (* idx 4))
          crate (get crate-slice-data pos)]
      (if crate
        (if (not= \space crate)
          (recur (update stacks idx conj crate) (inc idx))
          (recur stacks (inc idx)))
        stacks))))

(defn parse-stacks
  [data]
  (let [[stack-data data] (str/split data #"\n\n")
        crate-slices (filter #(not= \1 (get % 1))
                             (str/split-lines stack-data))
        stacks (reduce parse-crate-slice [] (reverse crate-slices))]
    [data stacks]))

;;; old function - for comparison's sake
(comment
  (defn parse-stacks
    [data]
    (let [[stack-data data] (str/split data #"\n\n")
          ;; crate-slices contains horizontal "slices" of the crates,
          ;; and each crate is paired with its stack number
          crate-slices (for [line (str/split-lines stack-data)
                             :when (not= \1 (get line 1))]
                         (for [idx (range)
                               :let [pos (inc (* idx 4))]
                               :while (< pos (count line))
                               :when (not= \space (get line pos))]
                           [idx (get line pos)]))
          ;; we need to take those crate slices, and put them in the correct stack
          stacks (reduce (fn stackify-slice[sofar slice]
                           (reduce (fn stackify-crate[sofar' [idx crate]]
                                     (update sofar' idx conj  crate))
                                   sofar
                                   slice))
                         []
                         (reverse crate-slices))]
      [data stacks]))
  )

(defn parse-moves
  [data]
  (map (fn parse-move[s]
         (let [simplified (str/split (str/replace s #"^move (\d+) from (\d+) to (\d+)$" "$1 $2 $3") #"\s")
               [num from to] simplified]
           [(parse-long num) (dec (parse-long from)) (dec (parse-long to))]))
       (str/split-lines data)))

(defn load-data
  [filename]
  (let [data (slurp filename)
        [data stacks] (parse-stacks data)
        moves (parse-moves data)]
    {:stacks stacks :moves moves}))

(defn ^:dynamic make-move
  [stack [n from to]]
  (if (<= n 0)
    stack
    (let [crate (peek (get stack from))
          stack' (-> stack
                     (update from pop)
                     (update to conj crate))]
      (recur stack' [(dec n) from to]))))

(defn make-moves
  [{:keys [stacks moves]}]
   (reduce make-move stacks moves))

(defn top-crates
  [stacks]
  (str/join (map first stacks)))

(defn part1
  [filename]
  (top-crates (make-moves (load-data filename))))

(assert (= "CMZ" (part1 "day05.example")))
(assert (= "VCTFTJQCG" (part1 "day05.input")))


(defn  make-move-part2
  [stack [n from to]]
  (let [[crates remaining] (split-at n (get stack from))]
    (-> stack
        (assoc from remaining)
        (update to #(concat %2 %1) crates))))

(defn part2
  [filename]
  (binding [make-move make-move-part2]
    (part1 filename)))

(assert (= "MCD" (part2 "day05.example")))
(assert (= "GCFGLDNJZ" (part2 "day05.input")))
