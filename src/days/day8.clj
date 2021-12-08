(ns days.day8
  (:require [clojure.string :as str]

            [utils :refer [read-string-seq]]))


;; Part 1

(def dummy-input '("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
                   "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
                   "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
                   "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
                   "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
                   "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
                   "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
                   "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
                   "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
                   "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))
(def input (read-string-seq "day8"))

(defn parse-input [input]
  (sequence
    (comp
      (map #(str/split % #" \| "))
      (map (fn [pair]
             (map #(->> (str/split % #" ")
                        (map set))
                  pair))))
    input))

(defn count-unique-digits [pairs]
  (count
    (sequence
      (comp
        (mapcat second)
        (filter #(#{2 ; 1
                    4 ; 4
                    3 ; 7
                    7 ; 8
                    } (count %))))
      pairs)))

(defn part-1 [input]
  (-> input
      parse-input
      count-unique-digits))

(part-1 dummy-input) ; => 26
(part-1 input) ; => 512
