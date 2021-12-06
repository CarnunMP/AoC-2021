(ns days.day6
  (:require [utils :refer [read-string-seq]]))

(def dummy-input "3,4,3,1,2")
(def input (first (read-string-seq "day6")))

(defn parse-input [input-string]
  (sequence
    (comp
      (remove #(= \, %))
      (map #(Character/digit % 10)))
    input-string))

(defn- advance-day [timers]
  (reduce
    (fn [res timer]
      (let [zero? (zero? timer)]
        (cond-> res
          zero? (conj 6)
          zero? (conj 8)
          (not zero?) (conj (dec timer)))))
    '()
    timers))

(defn advance-n-days [timers n]
  (last (take (inc n) (iterate advance-day timers))))

(defn part-1 [input days]
  (-> input
      parse-input
      (advance-n-days days)
      count))

(part-1 dummy-input 18) ; => 26
(part-1 input 80) ; => 386536
