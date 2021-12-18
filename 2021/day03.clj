(ns day03
  (:require [clojure.string :as s]))

(defn load-bits
  [input]
  (->> (slurp input)
       (s/split-lines)))

(defn bits->frequencies
  [bits]
  (map frequencies
       (apply map list bits)))

(defn gamma-rate
  [freqs]
  (->> freqs
       (map (fn [freqs]
              (let [zero-n (get freqs \0 0)
                    one-n  (get freqs \1 0)]
                (if (> zero-n one-n)
                  \0
                  \1))))
       (apply str)
       (#(Integer/parseInt % 2))))

(defn epsilon-rate
  [freqs]
  (->> freqs
       (map (fn [freqs]
              (let [zero-n (get freqs \0 0)
                    one-n  (get freqs \1 0)]
                (if (< zero-n one-n)
                  \0
                  \1))))
       (apply str)
       (#(Integer/parseInt % 2))))

(defn part1
  [input]
  (let [bits (load-bits input)
        freqs (bits->frequencies bits)]
    (* (gamma-rate freqs)
       (epsilon-rate freqs))))

(println "Part 1:" (part1 "day03.input"))
