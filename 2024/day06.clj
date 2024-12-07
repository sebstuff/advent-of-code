(ns day06
  (:require [clojure.string :as str]))

(def char->guard-facing {\^ :up
                         \> :right
                         \v :down
                         \< :left})

(def guard-turns {:up :right
                  :right :down
                  :down :left
                  :left :up})

(defn parse-input
  [input]
  (let [grid (mapv vec (str/split-lines input))
        rows (count grid)
        cols (count (first grid))
        coords (for [row (range rows)
                     col (range cols)]
                 [row col])
        guard (loop [[coord & coords-rest] coords]
                (if-let [facing (char->guard-facing (get-in grid coord))]
                  {:facing facing :position coord}
                  (recur coords-rest)))
        obstacles (loop [[coord & coords-rest] coords
                         obstacles-sofar #{}]
                    (if coord
                      (let [obstacles-sofar' (if (= \# (get-in grid coord))
                                               (conj obstacles-sofar coord)
                                               obstacles-sofar)]
                        (recur coords-rest obstacles-sofar'))
                      obstacles-sofar))]
    {:guard guard
     :path []
     :unique-path #{}
     :obstacles obstacles
     :rows rows
     :cols cols}))

(defn next-position
  "Calculates the next position the guard would have."
  [guard]
  (let [{:keys [position facing]} guard
        [row col] position]
    (case facing
      :up [(dec row) col]
      :right [row (inc col)]
      :down [(inc row) col]
      :left [row (dec col)])))

(defn guard-on-map?
  [state]
  (let [{:keys [guard rows cols]} state
        [row col] (:position guard)]
    (and (< -1 row rows)
         (< -1 col cols))))

(defn guard-looping?
  [state]
  (not= (count (:path state))
        (count (:unique-path state))))

(defn simulate-step
  [state]
  (let [{:keys [guard obstacles] :as state} state
        next-position (next-position guard)]
    (if-not (obstacles next-position)
      (-> state 
          (update :path conj! {:direction (:facing guard)
                              :position (:position guard)})
          (update :unique-path conj! {:direction (:facing guard)
                                     :position (:position guard)})
          (assoc-in [:guard :position] next-position))
      (update-in state [:guard :facing] guard-turns))))

(defn simulate
  [state]
  (loop [state (-> state
                   (update :path transient)
                   (update :unique-path transient))]
    (let [state' (simulate-step state)]
      (if (and (guard-on-map? state')
               (not (guard-looping? state')))
        (recur state')
        (-> state'
            (update :path persistent!)
            (update :unique-path persistent!))))))

(defn part1
  [filename]
  (->> (slurp filename)
       (parse-input)
       (simulate)
       (:path)
       (map :position)
       (set)
       (count)))

(println (time (part1 "day06.input")))

(defn part2
  [filename]
  (let [state (parse-input (slurp filename))
        simulated-state (simulate state)
        ;; it only makes sense to place obstacles where the guard walked
        candidate-obstacle-positions (->> simulated-state
                                          (:unique-path)
                                          (map :position)
                                          (set)
                                          ;; can't place on guard start
                                          (#(disj % (get-in state [:guard :position]))))]
    (->> candidate-obstacle-positions
         (map (fn insert-obstacle[obstacle-position]
                (update state :obstacles conj obstacle-position)))
         (pmap simulate)
         (filter guard-looping?)
         (count))))

(println (time (part2 "day06.input")))
(shutdown-agents)
