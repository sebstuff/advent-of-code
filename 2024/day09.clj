(ns day09
  (:require [clojure.string :as str]))

(def free-block -1)

(defn load-disk-map
  [filename]
  (->> (slurp filename)
       (str/trim)
       (mapv #(Integer/parseInt (str %)))))

(defn disk-map->disk
  [disk-map]
  (vec
   (mapcat (fn [entry idx]
             (cond
               (even? idx)
               (repeat entry (quot idx 2))

               :else
               (repeat entry free-block)))
           disk-map
           (range (count disk-map)))))

(defn compact-blocks
  [disk]
  (loop [start 0
         end (dec (count disk))
         disk' (transient [])]
    (cond
      (> start end)
      (persistent! disk')

      (= free-block (get disk end))
      (recur start
             (dec end)
             disk')

      (not= free-block (get disk start))
      (recur (inc start)
             end
             (conj! disk' (get disk start)))

      :else
      (recur (inc start)
             (dec end)
             (conj! disk' (get disk end))))))

(defn checksum
  [disk]
  (->> disk
       (map #(if (= free-block %) 0 %))
       (map-indexed *)
       (reduce +)))

(defn part1
  [filename]
  (->> (load-disk-map filename)
       (disk-map->disk)
       (compact-blocks)
       (checksum)))

(println (time (part1 "day09.input")))

(defn compact-files
  [disk]
  (loop [end (dec (count disk))
         disk' disk]
    (cond
      (< end 0)
      disk'

      (= -1 (get disk' end))
      (recur (dec end) disk')

      :else
      (let [file-id (get disk' end)
            file-start-idx (.indexOf disk' file-id)
            free-required (inc (- end file-start-idx))
            free-start-idx (loop [idx 0
                                  amount-free 0]
                             (cond
                               (>= idx end)
                               -1

                               (= amount-free free-required)
                               (- idx amount-free)

                               (= free-block (get disk' idx))
                               (recur (inc idx) (inc amount-free))

                               :else
                               (recur (inc idx) 0)))]
        (if (not= -1 free-start-idx)
          ;; move the file & clear up the space it used
          (recur (dec file-start-idx)
                 (mapv (fn replace-file-id[existing-file-id idx]
                         (cond
                           ;; new spot
                           (<= free-start-idx idx (dec (+ free-start-idx free-required)))
                           file-id

                           ;; old spot
                           (<= file-start-idx idx end)
                           -1

                           :else
                           existing-file-id))
                       disk'
                       (range (count disk'))))
          (recur (dec file-start-idx)
                 disk'))))))

(defn part2
  [filename]
  (->> (load-disk-map filename)
       (disk-map->disk)
       (compact-files)
       (checksum)))

(println (time (part2 "day09.input")))
