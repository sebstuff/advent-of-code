(ns day02
  (:require [clojure.string :as str]))

(->> (slurp "day02.input")
     (str/split-lines)
     (map #(str/split % #" "))
     (reduce (fn [{:keys [position depth]} [operation amount-str]]
               (let [amount (Integer/parseInt amount-str)]
                 (case operation
                   "forward" {:position (+ position amount) :depth depth}
                   "up" {:position position :depth (- depth amount)}
                   "down" {:position position :depth (+ depth amount)}
                   )))
             {:position 0 :depth 0})
     ((fn [{:keys [position depth]}] (* depth position)))
     (println "Part 1:"))

(->> (slurp "day02.input")
     (str/split-lines)
     (map #(str/split % #" "))
     (reduce (fn [{:keys [position depth aim]} [operation amount-str]]
               (let [amount (Integer/parseInt amount-str)]
                 (case operation
                   "forward" {:position (+ position amount) :depth (+ depth (* aim amount)) :aim aim}
                   "up" {:position position :depth depth :aim (- aim amount)}
                   "down" {:position position :depth depth :aim (+ aim amount)}
                   )))
             {:position 0 :depth 0 :aim 0})
     ((fn [{:keys [position depth]}] (* depth position)))
     (println "Part 2:"))
