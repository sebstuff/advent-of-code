(ns day16_test
  (:require [clojure.core.match :as m]))

(load-file "./day16.clj")
(use 'day16)

(m/match (binary->number "110")
         6 true)


(m/match (header "1101001")
         [{:version 6 :type :literal-value :type-id 4} _] true)


(m/match (literal-value {} "101111111000101000")
         [{:literal-value 2021} _] true)


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

(assert (= 16
           (->> "8A004A801A8002F478"
                (hex->binary)
                (packet)
                (first)
                (part1))))
(assert (= 12
           (->> "620080001611562C8802118E34"
                (hex->binary)
                (packet)
                (first)
                (part1))))
(assert (= 23
           (->> "C0015000016115A2E0802F182340"
                (hex->binary)
                (packet)
                (first)
                (part1))))
(assert (= 31
           (->> "A0016C880162017C3686B18A3D4780"
                (hex->binary)
                (packet)
                (first)
                (part1))))



(assert (= 3
           (->> "C200B40A82"
                (hex->binary)
                (packet)
                (first)
                (value))))
(assert (= 54
           (->> "04005AC33890"
                (hex->binary)
                (packet)
                (first)
                (value))))
(assert (= 7
           (->> "880086C3E88112"
                (hex->binary)
                (packet)
                (first)
                (value))))
(assert (= 9
           (->> "CE00C43D881120"
                (hex->binary)
                (packet)
                (first)
                (value))))
(assert (= 1
           (->> "D8005AC2A8F0"
                (hex->binary)
                (packet)
                (first)
                (value))))
(assert (= 0
           (->> "F600BC2D8F"
                (hex->binary)
                (packet)
                (first)
                (value))))
(assert (= 0
           (->> "9C005AC2F8F0"
                (hex->binary)
                (packet)
                (first)
                (value))))
(assert (= 1
           (->> "9C0141080250320F1802104A08"
                (hex->binary)
                (packet)
                (first)
                (value))))
