(ns day19
  (:require [clojure.string :as str]))

(defn possible?
  [patterns design]
  (if (= "" design)
    true
    (loop [[pattern & patterns'] patterns]
      (cond
        (not pattern)
        false

        (str/starts-with? design pattern)
        (if (possible? patterns (subs design (count pattern)))
          true
          (recur patterns'))

        :else
        (recur patterns'))
      )))

(defn part1
  [filename]
  (let [[patterns-str whitespace & designs] (str/split-lines (slurp filename))
        patterns (str/split patterns-str #", ")]
    (->> designs
         (filter #(possible? patterns %))
         (count))))

(assert (= 6 (part1 "day19.example")))
(println (time (part1 "day19.input"))) ; 228


(def ways-make
  (memoize (fn [patterns design]
             (if (= "" design)
               1
               (loop [[pattern & patterns'] patterns
                      ways 0]
                 (cond
                   (not pattern)
                   ways

                   (str/starts-with? design pattern)
                   (let [new-ways (ways-make patterns (subs design (count pattern)))]
                     (recur patterns' (+ new-ways ways)))

                   :else
                   (recur patterns' ways)))))))

(defn part2
  [filename]
  (let [[patterns-str whitespace & designs] (str/split-lines (slurp filename))
        patterns (str/split patterns-str #", ")]
    (->> designs
         (map #(ways-make patterns %))
         (apply +))))

(assert (= 16 (part2 "day19.example")))
(println (time (part2 "day19.input"))) ; 584553405070389
