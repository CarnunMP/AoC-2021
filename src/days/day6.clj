(ns days.day6
  (:require [utils :refer [read-string-seq]]))


;; Part 1

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

(part-1 input 256) ; wayyyy too slow... then OOMs

; so we're gonna have to do something a little more clever
; hmm... frequency maps again?


;; Part 2

(defn- advance-day' [timer-freqs]
  (let [zeros (get timer-freqs 0 0)]
    (as-> timer-freqs res
      (dissoc res 0)
      (reduce (fn [res' [timer freq]]
                (merge res' {(max (dec timer) 0) freq}))
              {}
              res)
      (into {} res)
      (cond-> res
        zeros (update 6 (fnil + 0) zeros)
        zeros (update 8 (fnil + 0) zeros)))))

(defn advance-n-days' [timer-freqs n]
  (last (take (inc n) (iterate advance-day' timer-freqs))))

(defn total [freqs]
  (reduce + (vals freqs)))

(defn part-2 [input days]
  (-> input
      parse-input
      frequencies
      (advance-n-days' days)
      total))

(part-2 dummy-input 18) ; => 26
(part-2 dummy-input 80) ; => 5934 
(part-2 input 256) ; => 1732821262171

; almost instant!
