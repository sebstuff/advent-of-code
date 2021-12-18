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


(defn rating
  [bits i f]
   (let [i-bits (map #(nth % i) bits)
         freq-0 (count (filter #(= % \0) i-bits))
         freq-1 (count (filter #(= % \1) i-bits))
         keep-bit (f freq-0 freq-1)
         matching (filter #(= keep-bit (nth % i)) bits)]
     (if (= 1 (count matching))
       (Integer/parseInt (apply str (first matching)) 2)
       (recur matching (inc i) f))))

(defn oxygen-generator-rating
  [bits]
  (rating bits 0
          (fn [freq-0 freq-1]
            (if (>= freq-1 freq-0) \1 \0))))

(defn co2-scrubber-rating
  [bits]
  (rating bits 0
          (fn [freq-0 freq-1]
            (if (<= freq-0 freq-1) \0 \1))))

(defn part2
  [input]
  (let [bits (load-bits input)]
    (* (oxygen-generator-rating bits)
       (co2-scrubber-rating bits))))

(println "Part 2:" (part2 "day03.input"))
