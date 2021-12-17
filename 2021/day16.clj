(ns day16
  (:require [clojure.string :as string]
            [clojure.core.match :as m]
            [clojure.tools.trace :refer [deftrace]]))


(def hex-to-binary
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
   \F "1111"})

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
(m/match (binary->number "110")
         6 true)


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
(m/match (header "1101001")
         [{:version 6 :type :literal-value :type-id 4} _] true)


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
(m/match (literal-value {} "101111111000101000")
         [{:literal-value 2021} _] true)


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


(defn operator
  "parses 'operator' packet"
  [header binary]
  (let [[length-type & binary] binary
        [subpackets binary] (operator-subpackets length-type binary)]
    [(assoc header :subpackets subpackets) binary]))


(defn packet
  "parses a packet"
  [binary]
  (let [[header binary] (header binary)]
    (case (:type header)
      :operator (operator header binary)
      :literal-value (literal-value header binary))))

(m/match (packet "110100101111111000101000")
           [{:version 6 :type :literal-value :type-id 4
             :literal-value 2021} _] true)
(m/match (packet "00111000000000000110111101000101001010010001001000000000")
           [{:version 1 :type :operator :type-id 6
             :subpackets [{:type :literal-value :literal-value 10}
                          {:type :literal-value :literal-value 20}]} _] true)
(m/match (packet "11101110000000001101010000001100100000100011000001100000")
           [{:version 7 :type :operator :type-id 3
             :subpackets [{:type :literal-value :literal-value 1}
                          {:type :literal-value :literal-value 2}
                          {:type :literal-value :literal-value 3}]} _] true)

(defn part1
  [packet]
  (case (:type packet)
    :literal-value (:version packet)
    :operator (+ (:version packet)
                 (reduce + 0 (map part1 (:subpackets packet))))))
(assert (= 16
           (->> "8A004A801A8002F478"
                (string/trim)
                (mapcat hex-to-binary)
                (packet)
                (first)
                (part1))))
(assert (= 12
           (->> "620080001611562C8802118E34"
                (string/trim)
                (mapcat hex-to-binary)
                (packet)
                (first)
                (part1))))
(assert (= 23
           (->> "C0015000016115A2E0802F182340"
                (string/trim)
                (mapcat hex-to-binary)
                (packet)
                (first)
                (part1))))
(assert (= 31
           (->> "A0016C880162017C3686B18A3D4780"
                (string/trim)
                (mapcat hex-to-binary)
                (packet)
                (first)
                (part1))))

(->> (slurp "day16.input")
     (string/trim)
     (mapcat hex-to-binary)
     (packet)
     (first)
     (part1)
     (println))
