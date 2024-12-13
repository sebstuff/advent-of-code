(ns day09
  (:require [clojure.string :as str]))

(def free-marker -1)

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
               (repeat entry free-marker)))
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

      (= free-marker (get disk end))
      (recur start
             (dec end)
             disk')

      (not= free-marker (get disk start))
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
       (map #(if (= free-marker %) 0 %))
       (map-indexed *)
       (reduce +)))

(defn part1
  [filename]
  (->> (load-disk-map filename)
       (disk-map->disk)
       (compact-blocks)
       (checksum)))

(println (time (part1 "day09.input")))

(defn insert![coll val start len]
  (loop [coll coll
         idx start]
    (if (>= idx (+ start len))
      coll
      (recur (assoc! coll idx val)
             (inc idx)))))

(defn free-blocks
  [disk]
  (loop [idx 0
         amount-free 0
         free-blocks []]
    (cond
      (>= idx (count disk))
      (if (> amount-free 0)
        (conj free-blocks {:start (- idx amount-free)
                           :end (dec idx)
                           :amount amount-free})
        free-blocks)

      (= free-marker (get disk idx))
      (recur (inc idx) (inc amount-free) free-blocks)

      :else
      (if (> amount-free 0)
        (recur (inc idx)
               0
               (conj free-blocks {:start (- idx amount-free)
                                  :end (dec idx)
                                  :amount amount-free}))
        (recur (inc idx) 0 free-blocks)))))

(defn compact-files-original
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

                               (= free-marker (get disk' idx))
                               (recur (inc idx) (inc amount-free))

                               :else
                               (recur (inc idx) 0)))]
        (if (not= -1 free-start-idx)
          ;; move the file & clear up the space it used
          (recur (dec file-start-idx)
                 (-> disk'
                     (transient)
                     (insert! file-id free-start-idx free-required)
                     (insert! free-marker file-start-idx free-required)
                     (persistent!)))
          (recur (dec file-start-idx)
                 disk'))))))

(defn find-free-block-idx
  [free-blocks amount before-idx]
  (some (fn [idx]
          (if (and (< (:start (get free-blocks idx)) before-idx)
                   (<= amount (:amount (get free-blocks idx))))
            idx
            nil))
        (range (count free-blocks))))

(defn update-free-blocks
  [free-blocks idx amount]
  (let [free-block (get free-blocks idx)]
    (if (= amount (:amount free-blocks))
      (filterv #(not= (:start free-block) (:start %))
               free-blocks)
      (-> free-blocks
          (update-in [idx :start] + amount)
          (update-in [idx :amount] - amount )))))

;; roughly cuts runtime in half
;; it keeps a list of where free blocks are found, and uses that to find them
(defn compact-files'
  [disk]
  (loop [end (dec (count disk))
         free-blocks (free-blocks disk)
         disk' disk]
    (cond
      (< end 0)
      disk'

      (= -1 (get disk' end))
      (recur (dec end) free-blocks disk')

      :else
      (let [file-id (get disk' end)
            file-start-idx (.indexOf disk' file-id)
            free-required (inc (- end file-start-idx))
            free-block-idx (find-free-block-idx free-blocks free-required end)]
        (if free-block-idx
          (let [free-block (get free-blocks free-block-idx)
                free-start-idx (:start free-block)]
            (recur (dec file-start-idx)
                   (update-free-blocks free-blocks free-block-idx free-required)
                   (-> disk'
                       (transient)
                       (insert! file-id free-start-idx free-required)
                       (insert! free-marker file-start-idx free-required)
                       (persistent!))))
          (recur (dec file-start-idx)
                 free-blocks
                 disk'))))))

(defn part2
  [filename]
  (->> (load-disk-map filename)
       (disk-map->disk)
       (compact-files')
       (checksum)))

(assert (= 2858 (part2 "day09.example")))
(println (time (part2 "day09.input")))
