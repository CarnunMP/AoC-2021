(ns days.day8
  (:require [clojure.string :as str]
            [clojure.set :as set]

            [utils :refer [read-string-seq]]))


;; Part 1

(def single-dummy-input '("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))
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

(str '(1 2 3 4))


;; Part 2

; some observations:
; - 1 (two segments) + 7 (three segments, two overlapping with one) -> top segment is difference
;     - and once this is known, it can be marked
; - bottom segment is the only one which appears in every non-immediately-identifiable number (i.e. every non- 1, 4, 7, or 8), except for top (already known)
; - take the 5-segment nums: 3 has all but one segment in common with 2, and all but a different one in common with 5
;     - 2 and 5 have two segments difference
;     -> gives us middle segment, cos we know which one is the 3 (and we already know top, bottom, and which two are either of top and bottom right)
; ...
; - hmm. this is all well and good so far, but really what we wanna produce is a mapping from sets of signals directly to digits. not to particular display patterns.
;   cos then we'd have to map again from display patterns to digits, hmm.
;     - so let's see if we can do that instead!
; - right off the bat we have the sets corresponding to 1, 4, 7, and 8.
; - and we know 3, as per the above
; - 9 is the six-segment digit with one segment difference from 3; so we have 9
; - taking the remaining five-segment numbers: 4 and 2 have two segments in common, whilst 4 and 5 have three.
;     - which gives us 2 and 5.
; - 5 and 6 have five segments in common; 5 and 0 have four.
;     - which gives us 0 and 6 -- and we're done!

; Let's code it up!

(defn- mark-1-4-7-8 [[signals output]]
  (let [segments->digit {2 1
                         4 4
                         3 7
                         7 8}]
    {:signal->digit (reduce
                      (fn [m signal]
                        (let [segment-count (count signal)]
                          (if (#{2 ; 1
                                 4 ; 4
                                 3 ; 7
                                 7 ; 8
                                 } segment-count)
                            (assoc m signal (segments->digit segment-count))
                            m)))
                      {}
                      signals)
     :signals signals
     :output output}))

(defn- mark-3 [{:keys [signals] :as m}]
  (let [[five-segment-0 five-segment-1 five-segment-2 :as five-segment-signals] (vec (filter #(= 5 (count %)) signals))
        overlaps {#{0 1} (set/intersection five-segment-0 five-segment-1)
                  #{0 2} (set/intersection five-segment-0 five-segment-2)
                  #{1 2} (set/intersection five-segment-1 five-segment-2)}
        three-signal (->> overlaps
                          (keep (fn [[k v]] (when (= 4 (count v)) k)))
                          (apply set/intersection)
                          first
                          (get five-segment-signals))]
    (assoc-in m [:signal->digit three-signal] 3)))

(defn- mark-9 [{:keys [signals signal->digit] :as m}]
  (let [six-segment-signals (filter #(= 6 (count %)) signals)
        three-signal (get (set/map-invert signal->digit) 3)
        nine-signal (first (filter #(= 5 (count (set/intersection three-signal %))) six-segment-signals))]
    (assoc-in m [:signal->digit nine-signal] 9)))

(defn- mark-2-5 [{:keys [signals signal->digit] :as m}]
  (let [digit->signal (set/map-invert signal->digit)
        two-and-five (->> signals
                          (filter #(and (= 5 (count %))
                                        (not= (digit->signal 3) %))))
        two (first (filter #(= 2 (count (set/intersection (digit->signal 4) %))) two-and-five))
        five (first (filter #(not= two %) two-and-five))]
    (update m :signal->digit merge {two 2
                                    five 5})))

(defn- mark-0-6 [{:keys [signals signal->digit] :as m}]
  (let [zero-and-six (set/difference (set signals) (set (keys signal->digit)))
        five (get (set/map-invert signal->digit) 5)
        zero (first (filter #(= 4 (count (set/intersection % five))) zero-and-six))
        six (first (filter #(not= zero %) zero-and-six))]
    (update m :signal->digit merge {zero 0
                                    six 6})))

(defn decode-outputs-and-sum [input]
  (transduce
    (comp
      (map mark-1-4-7-8)
      (map mark-3)
      (map mark-9)
      (map mark-2-5)
      (map mark-0-6)
      (map (fn [{:keys [signal->digit output]}]
             (map signal->digit output)))
      (map #(apply str %))
      (map #(Integer/parseInt %)))
    +
    0 input))


(defn part-2 [input]
  (->> input
       parse-input
       decode-outputs-and-sum))

(part-2 single-dummy-input) ; => 5353
(part-2 dummy-input) ; => 61229
(part-2 input) ; => 1091165

; wahey!
; not the prettiest code, but it does the job.
; and pretty quickly, too! :)
