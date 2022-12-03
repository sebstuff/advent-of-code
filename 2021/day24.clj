(ns day24
  (:require [clojure.string :as str]))

(defn make-system
  [input]
  {:input (map #(parse-long (str %)) (char-array input))
   :registers {"w" 0 "x" 0 "y" 0 "z" 0}})

(defn read-input
  [system]
  (let [[in & rest] (:input system)]
    [in (assoc system :input rest)]))

(defn write-register
  [system register value]
  (assoc-in system [:registers register] value))

(defn parse-value
  [system register-or-value]
  (if-let [value ((:registers system) register-or-value)]
    value
    (parse-long register-or-value)))

(defmulti execute
  (fn [system operation] (:instruction operation)))
(defn std-execute
  [system args f]
  (let [register (first args)]
    (write-register system
                    register
                    (f (parse-value system register) (parse-value system (second args))))))
(defmethod execute :inp
  [system {:keys [args]}]
  (let [[register] args
        [in system] (read-input system)]
    (write-register system register in)))
(defmethod execute :add
  [system {:keys [args]}]
  (std-execute system args +))
(defmethod execute :mul
  [system {:keys [args]}]
  (std-execute system args *))
(defmethod execute :div
  [system {:keys [args]}]
  (std-execute system args #(int (/ %1 %2))))
(defmethod execute :mod
  [system {:keys [args]}]
  (std-execute system args mod))
(defmethod execute :eql
  [system {:keys [args]}]
  (std-execute system args #(if (= %1 %2) 1 0)))

(defn execute-inp
  [system {:keys [args]}]
  (let [[register] args
        [in system] (read-input system)]
    (write-register system register in)))
(defn execute-add
  [system {:keys [args]}]
  (std-execute system args +))
(defn execute-mul
  [system {:keys [args]}]
  (std-execute system args *))
(defn execute-div
  [system {:keys [args]}]
  (std-execute system args #(int (/ %1 %2))))
(defn execute-mod
  [system {:keys [args]}]
  (std-execute system args mod))
(defn execute-eql
  [system {:keys [args]}]
  (std-execute system args #(if (= %1 %2) 1 0)))

#_(defn woot [] (println "hi"))
#_(eval (read-string "(woot)"))
#_((resolve (symbol "+")) 2 3)
(defn parse-operation
  [operation]
  (let [[instruction & args] (str/split operation #" ")]
    {:instruction (keyword instruction)#_(resolve (symbol (str "execute-"instruction)))
     :args args}))

(defn load-data
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map parse-operation)))

(defn run-program
  [operations input]
  (let [system (make-system input)]
    #_(reduce #((:instruction %2) %1 %2) system operations)
    (reduce #(execute %1 %2) system operations)))

(defn model-numbers
  []
  ;(map - (range -99999999999999 -11111111111111)))
  (for [d01 (reverse (range 1 10))
        d02 (reverse (range 1 10))
        d03 (reverse (range 1 10))
        d04 (reverse (range 1 10))
        d05 (reverse (range 1 10))
        d06 (reverse (range 1 10))
        d07 (reverse (range 1 10))
        d08 (reverse (range 1 10))
        d09 (reverse (range 1 10))
        d10 (reverse (range 1 10))
        d11 (reverse (range 1 10))
        d12 (reverse (range 1 10))
        d13 (reverse (range 1 10))
        d14 (reverse (range 1 10))]
    (str d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14)))

(defn find-model-number
  [operations [current & rest] start-time]
  (println "Working on" current "time" (- (System/currentTimeMillis) start-time))
  (let [system (run-program operations current)]
    (if (= 0 (get (:registers system) "z"))
      current
      (recur operations rest start-time))))

(defn part1
  [filename]
  (find-model-number (load-data filename)
                     (model-numbers)
                     (System/currentTimeMillis)))

#_(part1 "day24.example1" "3")
#_(part1 "day24.example2" "34")
#_(part1 "day24.example3" "5")
#_(part1 "day24.input" "5")

(part1 "day24.input")
