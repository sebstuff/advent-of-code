(ns day07
  (:require [clojure.string :as str]))

(defn process-command-cd
  [term dir]
  (case dir
    ".." (update term :path pop)
    "/" (dissoc term :path)
    (update term :path conj dir)))

(defn process-command
  [term [cmd arg]]
  (case cmd
    "cd" (process-command-cd term arg)
    "ls" term))

(defn process-input
  [term input]
  (let [[token1 & rest] (str/split input #" ")]
    (cond
      (= "$" token1) (process-command term rest)
      (= "dir" token1) (update-in term
                                  [:sizes (conj (:path term) (first rest))]
                                  (fn [v] (if v v 0)))
      :else
      (loop [term term
             path (or (:path term) '())]
        (let [term' (update-in term
                               [:sizes path]
                               (fn [v] (+ (or v 0) (parse-long token1))))]
          (if (seq path)
            (recur term' (pop path))
            term'))))))

(defn process-file
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (reduce process-input {})))

(defn part1
  [filename]
  (let [term (process-file filename)]
    (->> (:sizes term)
         (filter (fn [[k v]] (<= v 100000)))
         (map second)
         (reduce +))))

(assert (= 95437 (part1 "day07.example")))
(assert (= 2104783 (part1 "day07.input")))

(defn part2
  [filename]
  (let [term (process-file filename)
        space-free (- 70000000 (get-in term [:sizes '()]))
        space-required (- 30000000 space-free)]
    (->> (:sizes term)
         (filter (fn [[k v]] (>= v space-required)))
         (map second)
         (sort)
         (take 1)
         (first))))

(assert (= 24933642 (part2 "day07.example")))
(assert (= 5883165 (part2 "day07.input")))
