;;;; Solve for time_held in the equation:
;;;;  record_distance+1 = time_held * (total_time - time_held)
;;;;  record_distance+1 = time_held * total_time - time_held^2
;;;;  time_held^2 - time_held * total_time + record_distance+1 = 0
;;;;
;;;; This is now of the form:
;;;;   ax^2 + bx + c = 0
;;;;
;;;; Using:
;;;;  x = -b +- sqrt(b^2 - 4ac) / 2a
;;;;  time_held = -total_time +- sqrt(total_time^2 - 4 * record_distance+1 * 1) / (2 * 1)
;;;;
;;;; This gives us the low/high times.

(ns day06
  (:require [clojure.string :as str]))

(defn parse-input
  [s]
  (let [[_ times distances] (re-find #"Time: +([\d ]+)\nDistance: +([\d ]+)" s)
        times (map parse-long (str/split times #" +"))
        distances (map parse-long (str/split distances #" +"))]
    (mapv (fn [pair]
            (zipmap [:time :distance] pair))
          (map vector times distances))))

(defn ways-to-win
  [{:keys [time distance] :as race}]
  (let [hi-time-held (+ (/ time 2)
                        (/ (Math/sqrt (- (* time time) (* 4 (inc distance)))) 2))
        lo-time-held (- (/ time 2)
                        (/ (Math/sqrt (- (* time time) (* 4 (inc distance)))) 2))
        ;; if the lo time is a whole number, then we need to add one to the number of ways to win
        add-extra-one (if (= lo-time-held (Math/floor lo-time-held))
                        1
                        0)]
    (+ (- (long hi-time-held)
          (long lo-time-held))
       add-extra-one)))

(defn part1
  [filename]
  (->> (slurp filename)
       (parse-input)
       (map ways-to-win)
       (apply *)))

(assert (= 288 (part1 "day06.example")))
(assert (= 2612736 (part1 "day06.input")))

(defn parse-input-2
  [s]
  (let [[_ time distance] (re-find #"Time: +([\d ]+)\nDistance: +([\d ]+)" s)
        time (str/replace time #" +" "")
        distance (str/replace distance #" +" "")]
    {:time (parse-long time)
     :distance (parse-long distance)}))

(defn part2
  [filename]
  (->> (slurp filename)
       (parse-input-2)
       (ways-to-win)))

(assert (= 71503 (part2 "day06.example")))
(assert (= 29891250 (part2 "day06.input")))
