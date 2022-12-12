(ns day11
  (:require [clojure.string :as str]))

(def ^:dynamic *worry-filter* nil)

(defn parse-monkey
  [s]
  (let [[_ monkey-id items arg1 op arg2 test-divisor true-monkey false-monkey]
        (first (re-seq #"Monkey (\d+):\n  Starting items: ([\d, ]+)\n  Operation: new = ([^ ]+) ([^ ]+) ([^\n]+)\n  Test: divisible by (\d+)\n    If true: throw to monkey (\d+)\n    If false: throw to monkey (\d+)" s))]
    {:monkey-id (parse-long monkey-id)
     :items (mapv parse-long (str/split items #", "))
     :operation-fn (let [op-fn ({"+" + "*" *} op)]
                     (cond
                       (and (= "old" arg1) (= "old" arg2))
                       (fn operation-1[old] (op-fn old old))

                       (= "old" arg1)
                       (fn operation-2[old] (op-fn old (parse-long arg2)))

                       (= "old" arg2)
                       (fn operation-3[old] (op-fn (parse-long arg1) old))

                       :else
                       (throw (ex-info "Could not parse operation"
                                       {:arg1 arg1 :op op :arg2 arg2}))))
     :test-fn (fn test[item] (= 0 (mod item (parse-long test-divisor))))
     :divisor (parse-long test-divisor)
     :inspected 0
     :true-monkey (parse-long true-monkey)
     :false-monkey (parse-long false-monkey)}))

(defn monkey-tosses-to
  "returns the monkey the item gets tossed to"
  [monkey item]
  (if ((:test-fn monkey) item)
    (:true-monkey monkey)
    (:false-monkey monkey)))

(defn take-turn
  [monkeys idx]
  (let [monkey (get monkeys idx)
        [item & items] (:items monkey)]
    (if item
      (let [item' ((:operation-fn monkey) item)
            item' (*worry-filter* item')
            next-monkey (monkey-tosses-to monkey item')]
        (recur (-> monkeys
                   (assoc-in [idx :items] (vec items))
                   (update-in [idx :inspected] inc)
                   (update-in [next-monkey :items] conj item'))
               idx))
      monkeys)))

(defn process-round
  [monkeys]
  (reduce take-turn
          monkeys
          (range (count monkeys))))

(defn load-monkeys
  [filename]
  (mapv parse-monkey (str/split (slurp filename) #"\n\n")))

(defn run-rounds
  [monkeys rounds]
  (if (= 0 rounds)
    monkeys
    (recur (process-round monkeys) (dec rounds))))

(defn part1
  [filename]
  (binding [*worry-filter* #(int (/ % 3))]
    (let [monkeys (load-monkeys filename)
          monkeys (run-rounds monkeys 20)]
      (->> monkeys
           (sort-by :inspected #(compare %2 %1))
           (take 2)
           (map :inspected)
           (apply *)))))

(assert (= 10605 (part1 "day11.example")))
(assert (= 151312 (part1 "day11.input")))

(defn part2
  [filename]
  (let [monkeys (load-monkeys filename)
        divisor-product (apply * (map :divisor monkeys))]
    (binding [*worry-filter* #(mod % divisor-product)]
      (->> (run-rounds monkeys 10000)
           (sort-by :inspected #(compare %2 %1))
           (take 2)
           (map :inspected)
           (apply *)))))

(assert (= 2713310158 (part2 "day11.example")))
(assert (= 51382025916 (part2 "day11.input")))
