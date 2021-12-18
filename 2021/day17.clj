(ns day17
  (:require [clojure.string :as string]
            [clojure.core.match :as match]))

(defn step
  [{{x  :x y  :y} :position
    {vx :x vy :y} :velocity}]
  (let [new-x (+ x vx)
        new-y (+ y vy)
        new-vx (cond
                 (> vx 0) (dec vx)
                 (< vx 0) (inc vx)
                 :else 0)
        new-vy (dec vy)]
    {:position {:x new-x  :y new-y}
     :velocity {:x new-vx :y new-vy}}))


(defn hits-target?
  [{{x :x y :y} :position :as probe}
   {[min-y max-y] :y [min-x max-x] :x :as target}]
  (cond
    (and (<= min-x x max-x)
         (<= min-y y max-y))
    true

    (or (> x max-x)
        (< y min-y))
    false

    :else
    (recur (step probe) target)))


(defn velocities-hit
  [target]
  (let [min-vx 1
        max-vx (second (:x target))
        min-vy (apply min (:y target))
        max-vy (-' min-vy)
        velocities (for [vx (range min-vx (inc max-vx))
                         vy (range min-vy (inc max-vy))]
                     {:x vx :y vy})]
    (filter (fn [velocity]
              (let [probe {:position {:x 0 :y 0}
                           :velocity velocity}]
                (hits-target? probe target)))
              velocities)))

(defn highest-probe
  [target]
  (->> target
       velocities-hit
       (sort-by #(- (:y %)))
       (first)))


(defn parse-target
  [input]
  (let [[_ min-x max-x min-y max-y] (re-matches
                                     #"^target area: x=(.+)\.\.(.+), y=(.+)\.\.(.+)$"
                                     input)]
    {:x [(Integer/parseInt min-x) (Integer/parseInt max-x)]
     :y [(Integer/parseInt min-y) (Integer/parseInt max-y)]}))


(defn top-height
  [{:keys [y]}]
  (/ (* y (inc y)) 2))


(defn part1
  [input]
  (-> input
      parse-target
      highest-probe
      top-height))

(defn part2
  [input]
  (-> input
      parse-target
      velocities-hit
      count))

(def example "target area: x=20..30, y=-10..-5")
(def my-input "target area: x=150..171, y=-129..-70")

(time (println "part 1:" (part1 my-input)))
(time (println "part 2:" (part2 my-input)))
