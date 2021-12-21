(ns day18
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.zip :as zip]))

(defn add
  [n1 n2]
  [n1 n2])


(defn split
  [n]
  (loop [z (zip/vector-zip n)]
    (cond
      (zip/end? z)
      nil

      (and (number? (zip/node z))
           (> (zip/node z) 9))
      (-> z
          (zip/edit (fn [value]
                      [(int (Math/floor (/ value 2)))
                       (int (Math/ceil (/ value 2)))]))
          (zip/root))

      :else
      (recur (zip/next z)))))


(defn walk-back
  [z f times]
  (if (> times 0)
    (reduce (fn [z' _z] (f z'))
            z
            (range times))
    z))


(defn add-left
  [z amt]
  (loop [z z
         times 0]
    (let [z' (zip/prev z)]
      (cond
        (nil? z')
        (walk-back z zip/next times)

        (vector? (zip/node z'))
        (recur z' (inc times))

        :else
        (walk-back (zip/edit z' + amt)
                zip/next
                (inc times))))))

(defn add-right
  [z amt]
  (loop [z z
         times 0]
    (let [z' (zip/next z)]
      (cond
        (zip/end? z')
        (walk-back z zip/prev times)

        (vector? (zip/node z'))
        (recur z' (inc times))

        :else
        (walk-back (zip/edit z' + amt)
                zip/prev
                times)))))

(defn explode
  [n]
  (loop [z (zip/vector-zip n)]
    (cond
      (zip/end? z)
      nil

      (and (vector? (zip/node z))
           (>= (count (zip/path z)) 4))
      (let [[left-n right-n] (zip/node z)]
        (-> z
            (zip/replace 0)
            (add-left left-n)
            (add-right right-n)
            (zip/root)))

      :else
      (recur (zip/next z)))))

(assert (= [[[[0,9],2],3],4]
            (explode [[[[[9,8],1],2],3],4])))
(assert (= [7,[6,[5,[7,0]]]]
           (explode [7,[6,[5,[4,[3,2]]]]])))
(assert (= [[6,[5,[7,0]]],3]
           (explode [[6,[5,[4,[3,2]]]],1])))
(assert (= [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
           (-> [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] explode)))
(assert (= [[3,[2,[8,0]]],[9,[5,[7,0]]]]
           (-> [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] explode explode)))

(defn sf-reduce
  [n]
  (if-let [exploded-n (explode n)]
    (do
      (recur exploded-n))
    (if-let [split-n (split n)]
      (do
        (recur split-n))
      (do
        n))))

(assert (= [[3,[2,[8,0]]],[9,[5,[7,0]]]]
           (sf-reduce [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])))
(assert (= [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
           (sf-reduce (add [[[[4,3],4],4],[7,[[8,4],9]]] [1,1]))))

(defn magnitude
  [n]
  (if (vector? n)
    (let [[l r] n
          l-mag (magnitude l)
          r-mag (magnitude r)]
      (+ (* 3 l-mag)
         (* 2 r-mag)))
    n))

(assert (= 143 (magnitude [[1,2],[[3,4],5]])))
(assert (= 1384 (magnitude [[[[0,7],4],[[7,8],[6,0]]],[8,1]])))
(assert (= 445 (magnitude [[[[1,1],[2,2]],[3,3]],[4,4]])))
(assert (= 791 (magnitude [[[[3,0],[5,3]],[4,4]],[5,5]])))
(assert (= 1137 (magnitude [[[[5,0],[7,4]],[5,5]],[6,6]])))
(assert (= 3488 (magnitude [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])))

(defn part1
  [filename]
  (->> (slurp filename)
       (string/split-lines)
       (map edn/read-string)
       (reduce (fn [sofar n]
                 (sf-reduce (add sofar n))))
       (magnitude)))

(println "part1" (part1 "day18.input"))
