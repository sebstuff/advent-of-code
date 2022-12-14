(ns day10
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn make-device
  [instructions]
  {:instructions instructions
   :register 1
   :cycle 0
   :running nil})

(defn cycle-noop
  [device]
  (assoc device :running nil))

(defn cycles-to-run
  [instruction]
  (if (= "noop" instruction)
    1
    2))

(defn start-cycle
  "Performs operations done at start of cycle"
  [{:keys [instructions running] :as device}]
  (let [device' (update device :cycle inc)]
    (if running
      device'
      (let [[run & instructions'] instructions]
        (if run
          (-> device'
              (assoc :instructions instructions')
              (assoc :running [run (cycles-to-run run)]))
          device')))))

(defn during-cycle
  "Performs operations done during the cycle"
  [device]
  device)

(defn run-instruction
   [device instruction]
  (cond
    (= "noop" instruction) device
    :else (let [[_ amount] (str/split instruction #" ")
                amount (parse-long amount)]
            (update device :register + amount))))

(defn end-cycle
  "Performs operations done at the end of the cycle"
  [{:keys [running cycle] :as device}]
  (if (= cycle 0)
    device
    (let [[instruction cycles-left] running
          cycles-left' (dec cycles-left)]
      (if (= 0 cycles-left')
        (-> device
            (run-instruction instruction)
            (assoc :running nil))
        (assoc-in device [:running 1] cycles-left')))))

(defn cycle
  [device]
  (->> device
       ;; since we need to observe the during-cycle, move the end-cycle to the start of a cycle
       (end-cycle)
       (start-cycle)
       (during-cycle)))

(defn load-instructions
  [filename]
  (->> (slurp filename)
       (str/split-lines)))

(defn run-instructions
  [device]
  (rest (take-while #(or (seq (:instructions %))
                         (:running %)) (iterate cycle device))))

(defn part1
  [filename]
  (let [device (make-device (load-instructions filename))
        devices (run-instructions device)]
    (->> devices
         (map #(select-keys % [:cycle :register]))
         (filter #(or (= 0 (mod (- (:cycle %) 20) 40))
                      (= 20 (:cycle %))))
         (map #(apply * (vals %)))
         (apply +))))

(assert (= 13140 (part1 "day10.example")))
(assert (= 11820 (part1 "day10.input")))


(defn render-pixel
  [line [register pixel]]
  (if (#{pixel (dec pixel) (inc pixel)} register)
    (str line \#)
    (str line \.)))

(defn render-line
  [registers]
  (reduce render-pixel "" (map #(vec [%1 %2]) registers (range))))

(defn part2
  [filename]
  (let [device (make-device (load-instructions filename))
        devices (run-instructions device)]
    (->> devices
         (map :register)
         (partition 40)
         (map render-line)
         (str/join "\n"))))

(println (part2 "day10.example"))
(println)
(println (part2 "day10.input"))
