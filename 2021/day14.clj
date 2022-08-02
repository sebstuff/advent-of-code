(ns day14
  (:require [clojure.string :as string]))


(defn parse-input
  [input]
  (let [[template & rules] (->> input
                                string/split-lines
                                (filter #(not= "" %)))
        rules (into {} (map #(string/split % #" -> ") rules))]
    {:template template
     :rules rules}))


(defn calculate
  [template]
  (let [quantities (->> template
                        (group-by identity)
                        (vals)
                        (map count))
        max-value (apply max quantities)
        min-value (apply min quantities)]
    (- max-value min-value)))


(defn apply-rules
  ([data] (apply-rules data 1))
  ([{:keys [template rules] :as data} times]
   (let [new-rules
         {:template (apply str
                           (map (fn [f s]
                                  (let [char (get rules (str f s))]
                                    (if char
                                      (str f char)
                                      (str f))))
                                template (rest (concat template "_"))))
          :rules rules}]
     (if (= times 1)
       new-rules
       (recur new-rules (dec times))))))


(defn run
  [filename times]
  (->> filename
       slurp
       parse-input
       (#(apply-rules % times))
       (#(calculate (:template %)))))


(assert (= 1588 (run "day14.example" 10)))
(println "part1:" (run "day14.input" 10))


; this is too slow
#_(assert (= 2188189693529 (run "day14.example" 40)))
#_(println "part2:" (run "day14.input" 10))
#_(time (println "part2:" (run "day14.example" 20)))
