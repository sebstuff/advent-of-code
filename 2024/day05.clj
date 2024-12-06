(ns day05
  (:require [clojure.string :as str]))

(defn parse-rules
  [rules-str]
  (set
   (map (fn parse-rule[s]
          (let [[before after] (str/split s #"\|")]
            [(Integer/parseInt before)
             (Integer/parseInt after)]))
        (str/split-lines rules-str))))

(defn parse-updates
  [updates-str]
  (map (fn parse-update[s]
         (->> (str/split s #",")
              (mapv Integer/parseInt)))
       (str/split-lines updates-str)))

(defn parse-input
  [input]
  (let [[rules-str updates-str] (str/split input #"\r?\n\r?\n|\r")]
    {:rules (parse-rules rules-str)
     :updates (parse-updates updates-str)}))

(defn rules-followed?
  [rules update]
  (loop [[page & pages-rest] update]
    (if page
      (if (not-any? rules
                    (for [page2 pages-rest]
                      [page2 page]))
        (recur pages-rest)
        false)
      true)))

(defn part1
  [filename]
  (let [{:keys [rules updates]} (parse-input (slurp filename))]
    (->> updates
         (filter #(rules-followed? rules %))
         (map (fn middle-page[update] (nth update (quot (count update) 2))))
         (reduce + 0))))

(println (time (part1 "day05.input")))

(defn fix-update
  [update rules]
  (reduce (fn insert-page[fixed-update-sofar page]
            (if-let [first-rule-violation-idx (first (filter (fn rules-violated?[idx]
                                                               (rules [(get fixed-update-sofar idx) page]))
                                                             (range (count fixed-update-sofar))))]
              (let [[pages-left pages-right] (split-at first-rule-violation-idx fixed-update-sofar)]
                (vec (concat pages-left [page] pages-right)))
              (conj fixed-update-sofar page)))
          []
          update))

(defn part2
  [filename]
  (let [{:keys [rules updates]} (parse-input (slurp filename))]
    (->> updates
         (filter #(not (rules-followed? rules %)))
         (map #(fix-update % rules))
         (map (fn middle-page-number[page] (nth page (quot (count page) 2))))
         (reduce + 0))))

(println (time (part2 "day05.input")))
