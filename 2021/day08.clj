(ns day08
  (:require [clojure.string :as s]
            [clojure.set :as sets]
            [clojure.string :as str]))

(->> (slurp "day08.input")
     (s/split-lines)
     (map #(-> % (s/split #" \| ") (second) (s/split #" ")))
     (mapcat #(filter (fn filter-fn[chars] (get #{2 4 3 7} (count chars))) %))
     (count)
     (println "Part 1:"))

; 0 -> abc efg 6
; 1 ->   c  f  2
; 2 -> a cde g 5
; 3 -> a cd fg 5
; 4 ->  bcd f  4
; 5 -> ab d fg 5
; 6 -> ab defg 6
; 7 -> a c  f  3
; 8 -> abcdefg 7
; 9 -> abcd fg 6
;
; 1 ->   c  f  2
; 7 -> a c  f  3
; 4 ->  bcd f  4
; 5 -> ab d fg 5
; 2 -> a cde g 5
; 3 -> a cd fg 5
; 0 -> abc efg 6
; 6 -> ab defg 6
; 9 -> abcd fg 6
; 8 -> abcdefg 7
;
; {a 8, c 8, d 7, f 9, g 7, e 4, b 6}
;
; we know which one is 1, 4, 7, and 8 since their count is unique
; 1, 7: cf & acf, so we know which one is "a"
; f is the only in 9 numbers, so we know "f"
;   now we have a, f
; e is the only in 4 numbers, so we know e
;   now we have a, e, f
; b is the only in 6 numbers, so we know b
;   now we have a, b, e, f
; l2 uses c&f, we know f, so we know c
;   now we have a, b, c, e, f
; l4 uses bcdf, we know bcf, so we know d
;   now we have a, b, c, d, e, f
; we have all but 1, remaining is g



(def number-segments
  {"abcefg" 0
   "cf" 1
   "acdeg" 2
   "acdfg" 3
   "bcdf" 4
   "abdfg" 5
   "abdefg" 6
   "acf" 7
   "abcdefg" 8
   "abcdfg" 9})

; 1, 7: unique lengths, have cf & acf, so we know which one is "a"
(defn crack-a
  [decoder signals]
  (let [n1 (first (filter #(= 2 (count %)) signals))
        n7 (first (filter #(= 3 (count %)) signals))
        answer (first (sets/difference (set n7) (set n1)))]
    (assoc decoder answer \a)))

; f is the only in 9 numbers, so we know f
(defn crack-f
  [decoder signals]
  (let [all-letters (apply str signals)
        freqs (sets/map-invert (frequencies all-letters))
        answer (get freqs 9)]
    (assoc decoder answer \f)))

; e is the only in 4 numbers, so we know e
(defn crack-e
  [decoder signals]
  (let [all-letters (apply str signals)
        freqs (sets/map-invert (frequencies all-letters))
        answer (get freqs 4)]
    (assoc decoder answer \e)))

; b is the only in 6 numbers, so we know b
(defn crack-b
  [decoder signals]
  (let [all-letters (apply str signals)
        freqs (sets/map-invert (frequencies all-letters))
        answer (get freqs 6)]
    (assoc decoder answer \b)))

; l2 uses c&f, we know f, so we know c
(defn crack-c
  [decoder signals]
  (let [n1 (first (filter #(= 2 (count %)) signals))
        answer (first (sets/difference (set n1) (set (keys decoder))))]
    (assoc decoder answer \c)))

; l4 uses bcdf, we know bcf, so we know d
(defn crack-d
  [decoder signals]
  (let [n4 (first (filter #(= 4 (count %)) signals))
        answer (first (sets/difference (set n4) (set (keys decoder))))]
    (assoc decoder answer \d)))

; we have all but 1, remaining is g
(defn crack-g
  [decoder signals]
  (let [answer (first (sets/difference (set "abcdefg") (set (keys decoder))))]
    (assoc decoder answer \g)))

(defn crack-decoder
  [signals]
  (-> {}
      (crack-a signals)
      (crack-f signals)
      (crack-e signals)
      (crack-b signals)
      (crack-c signals)
      (crack-d signals)
      (crack-g signals)))

(defn decode-digit
  [decoder digit]
  (->> digit
       (map (fn [c] (get decoder c)))
       (sort)
       (str/join)
       (get number-segments)))

(defn decode-value
  [decoder value]
  (->> value
       (map #(decode-digit decoder %))
       (map str)
       (str/join)
       (Integer/parseInt)))

(let [decoders (->> (slurp "day08.input")
                    (s/split-lines)
                    (map #(-> % (s/split #" \| ") (first) (s/split #" ")))
                    (map crack-decoder))]
  (->> (slurp "day08.input")
       (s/split-lines)
       (map #(-> % (s/split #" \| ") (second) (s/split #" ")))
       (map decode-value decoders)
       (reduce +)
       (println "Part 2:")))
