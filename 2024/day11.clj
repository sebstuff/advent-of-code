(ns day11
  (:require [clojure.string :as str]))

(defn parse-pebbles
  [s]
  (-> s
      (str/trim)
      (str/split #" ")
      (#(mapv bigint %))))


(defn change-pebble
  [pebble]
  (if (= 0 pebble)
    [1]
    (let [pebble-str (str pebble)
          pebble-len (count pebble-str)]
      (if (even? pebble-len)
        [(bigint (apply str (take (/ pebble-len 2) pebble-str)))
         (bigint (apply str (drop (/ pebble-len 2) pebble-str)))]
        [(* 2024 pebble)]))))

(defn blink
  [pebbles]
  (mapcat change-pebble pebbles))

(defn blink-times
  [n pebbles]
  (loop [i 0
         pebbles' pebbles]
    (println "On time" i)
    (doall pebbles')
    (if (= n i)
      pebbles'
      (recur (inc i)
             (blink pebbles')))))

(defn solve
  [filename times]
  (->> (slurp filename)
       (parse-pebbles)
       (blink-times times)
       (count)))

(println (time (solve "day11.input" 25)))


#_(println (time (solve "day11.input" 75)))
