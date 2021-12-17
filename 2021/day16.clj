(ns day16
  (:require [clojure.string :as string]
            [clojure.tools.trace :refer [deftrace]]))


(defn hex->binary
  [hex]
  (let [conversion-table
        {\0 "0000"
         \1 "0001"
         \2 "0010"
         \3 "0011"
         \4 "0100"
         \5 "0101"
         \6 "0110"
         \7 "0111"
         \8 "1000"
         \9 "1001"
         \A "1010"
         \B "1011"
         \C "1100"
         \D "1101"
         \E "1110"
         \F "1111"}]
    (->> hex
         (string/trim)
         (mapcat conversion-table))))

(defn str-split-at
  [n binary]
  {:pre [(<= n (count binary))]}
  (let [[f r] (split-at n binary)]
    [(apply str f) (apply str r)]))

(defn binary->number
  "converts binary to number

  numbers have most significant bit first"
  [binary]
  (BigInteger. (apply str binary) 2))

(defn header
  "parses the packet header

  - first 3 bits is version
  - second 3 bits is type"
  [binary]
  (let [[version binary] (str-split-at 3 binary)
        [type binary] (str-split-at 3 binary)
        type-id (binary->number type)
        type (case type-id
               4 :literal-value
               :operator)]
    [{:type type
      :type-id type-id
      :version (binary->number version)}
     binary]))


(defn literal-value
  "parses 'literal value' packet

  literal values are numbers, encuded under the following rules:
  - the binary number is padded with zeroes, so its a multiple of 4
  - bits are split into groups of 4
  - those groups of 4 are prepended with a 1 or 0: 1 means there's another group, 0 means this is the last one"
  [header binary]
  (loop [value ""
         binary binary]
    (let [[[ch & bits] binary] (str-split-at 5 binary)
          value (str value (apply str bits))]
      (case ch
        \0 [(merge header {:literal-value (binary->number value)}) binary]
        \1 (recur value binary)))))


(declare packet)

(defn operator-subpackets
  "parses the subpackets of an operator packet"
  [length-type binary]
  (case length-type
    ;; a length type of 0 means the next 15 bits specify the total length, in
    ;; bits, of the subpackets within this packet
    \0 (let [[subpacket-bit-length-str binary] (str-split-at 15 binary)
             subpacket-bit-length (binary->number subpacket-bit-length-str)
             [subpacket-bits binary] (str-split-at subpacket-bit-length binary)]
         (loop [subpacket-bits subpacket-bits
                subpackets []]
           (let [[subpacket subpacket-bits] (packet subpacket-bits)
                 subpackets (conj subpackets subpacket)]
             (if (empty? subpacket-bits)
               [subpackets binary]
               (recur subpacket-bits subpackets)))))
    ;; a length type of 1 means the next 11 bits specify the total number of
    ;; subpackets immediately contained by this packet
    \1 (let [[subpacket-count binary] (str-split-at 11 binary)
             subpacket-count (binary->number subpacket-count)]
         (loop [subpackets []
                binary binary]
           (let [[subpacket binary] (packet binary)
                 subpackets (conj subpackets subpacket)]
             (if (= (count subpackets) subpacket-count)
               [subpackets binary]
               (recur subpackets binary)))))))

(defn operation
  "Given the packet's type-id, finds the operation"
  [type-id]
  (case type-id
    0 +
    1 *
    2 min
    3 max
    5 #(if (> %1 %2) 1 0)
    6 #(if (< %1 %2) 1 0)
    7 #(if (= %1 %2) 1 0)))

(defn operator
  "parses 'operator' packet"
  [header binary]
  (let [[length-type & binary] binary
        [subpackets binary] (operator-subpackets length-type binary)
        operation (operation (:type-id header))
        packet (merge header {:subpackets subpackets :operation operation})]
    [packet binary]))


(defn value
  "finds the value of a packet"
  [packet]
  (case (:type packet)
    :literal-value (:literal-value packet)
    :operator (let [subpackets (:subpackets packet)
                    subpacket-values (map value subpackets)
                    value (apply (:operation packet) subpacket-values)]
                value)))


(defn packet
  "parses a packet"
  [binary]
  (let [[header binary] (header binary)]
    (case (:type header)
      :operator (operator header binary)
      :literal-value (literal-value header binary))))


(defn part1
  [packet]
  (case (:type packet)
    :literal-value (:version packet)
    :operator (+ (:version packet)
                 (reduce + 0 (map part1 (:subpackets packet))))))

(->> (slurp "day16.input")
     (hex->binary)
     (packet)
     (first)
     (part1)
     (println "part1:"))

(->> (slurp "day16.input")
     (hex->binary)
     (packet)
     (first)
     (value)
     (println "part2:"))
