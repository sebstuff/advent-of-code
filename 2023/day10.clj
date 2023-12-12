(ns day10
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(def char->connections
  {\| #{:n :s}
   \- #{:e :w}
   \L #{:n :e}
   \J #{:n :w}
   \7 #{:s :w}
   \F #{:s :e}
   \S :start})

(defn reverse-direction
  [dir]
  (case dir
    :n :s
    :s :n
    :e :w
    :w :e))

(defn neighbors
  [[row col]]
  {:w [row (- col 1)]
   :e [row (+ col 1)]
   :s [(+ row 1) col]
   :n [(+ row -1) col]})
(assert (= {:w [0 -1] :e [0 1] :s [1 0] :n [-1 0]} (neighbors [0 0])))

(defn connected-neighbors
  [[row col] diagram]
  (let [neighbors (neighbors [row col])
        connection (get diagram [row col])]
    (into {} (filter #(connection (first %)) neighbors))))

(defn parse-input
  [s]
  (let [diagram (->> s
                     (str/split-lines)
                     (map-indexed (fn [row line]
                                    (map-indexed (fn [col ch]
                                                   [[row col] (char->connections ch)])
                                                 line)))
                     (mapcat identity)
                     (into {}))
        start-pos (->> diagram
                        (filter #(= :start (second %)))
                        (keys)
                        first)
        start-neighbors (neighbors start-pos)
        start-connections (into #{} (filter #(connects? #{(first %)} (get diagram (second %))) start-neighbors))
        diagram-replace-start (assoc diagram start-pos (into #{} (map first start-connections)))]
    {:start start-pos :diagram diagram-replace-start}))

(defn find-furthest-point
  [{:keys [start diagram]}]
  (loop [[pos & pos-queue] #{start}
         distances {start 0}]
    (let [connected-neighbors (->> (connected-neighbors pos diagram)
                                   (vals)
                                   (filter #(and (not (get pos-queue %)) (not (distances %)))))
          distances' (reduce (fn [distances neighbor]
                               (if (contains? distances neighbor)
                                 distances
                                 (assoc distances neighbor (inc (get distances pos)))))
                             distances
                             connected-neighbors)]
      (if (or (seq connected-neighbors) (seq pos-queue))
        (recur (concat pos-queue connected-neighbors) distances')
        (apply max (map second distances'))))))

(defn part1
  [filename]
  (let [{:keys [start diagram] :as input} (parse-input (slurp filename))]
    (find-furthest-point input)))

(assert (= 4 (time (part1 "day10.example"))))
(assert (= 8 (time (part1 "day10.example2"))))
(assert (= 6815 (time (part1 "day10.input"))))
