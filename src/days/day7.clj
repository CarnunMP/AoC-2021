(ns days.day7
  (:require [clojure.string :as str]

            [utils :refer [read-string-seq]]))


; Part 1

(def dummy-input "16,1,2,0,4,2,7,1,2,14")
(def input (first (read-string-seq "day7")))

(defn parse-input [input]
  (->> (str/split input #",")
       (map #(Integer/parseInt %))
       sort
       vec))

(defn assoc-median [nums]
  (let [middle-index (dec (/ (count nums) 2))]
    {:nums nums
     :medians (if (int? middle-index)
               (subvec nums middle-index (+ 2 middle-index))
               (subvec nums (int middle-index) (inc (int middle-index))))}))

(defn calculate-fuel-use [{:keys [nums medians]}]
  (letfn [(fuel-use [median]
            (reduce (fn [fuel-use n]
                      (+ fuel-use (Math/abs (- n median))))
                    0
                    nums))]
    (apply max (map fuel-use medians))))

(defn part-1 [input]
  (-> input
      parse-input
      assoc-median
      calculate-fuel-use))

(part-1 dummy-input) ; => 37
(part-1 input) ; => 328262


;; Part 2

; before: minimise sum of ds, where d is distance
; now: minimise sum of t(d) = d(d + 1)/2, as t(d) is a https://en.wikipedia.org/wiki/Triangular_number

; hmm. I can't think of anything cleverer than just computing this for a bunch of positions...
; and maybe stopping once the _next_ position has a fuel use greater than the previous—cos there will be a global minimum!
; there's a symmetry to things after all


(defn- fuel-use [n candidate-pos]
  (let [d (Math/abs (- n candidate-pos))]
    (/ (* d (inc d)) 2)))

(defn- total-fuel-use [nums candidate-pos]
  (reduce #(+ %1 (fuel-use %2 candidate-pos))
          0 nums))

(defn find-best-total-fuel-use [nums]
  (reduce
    (fn [best candidate-pos]
      (let [current (total-fuel-use nums candidate-pos)]
        (if (<= current best)
          current
          (reduced best))))
    ##Inf
    (range (first nums) (inc (peek nums)))))

(defn part-2 [input]
  (-> input
      parse-input
      find-best-total-fuel-use))

(part-2 input) ; => 90040997
