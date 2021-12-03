(ns days.day3
  (:require [utils :refer [read-string-seq]]))


;; Part 1
(def dummy-input '("00100"
                    "11110"
                    "10110"
                    "10111"
                    "10101"
                    "01111"
                    "00111"
                    "11100"
                    "10000"
                    "11001"
                    "00010"
                    "01010"))

(def dummy-col '(0 1 1 1 1 0 0 1 1 1 0 0))

(defn gamma-rate [input] ; TODO: re-write
  (->> input
       (map (fn [n] (map #(Character/digit % 2) n))) ; turn num strings into lists of ints
       (apply map (fn [& col]
                    (filter zero? col))) ; map over cols, keep 0s
       (map count) ; count 0s
       (map (fn [n]
              (if (< n (/ (count input) 2))
                "1"
                "0"))) ; if less than n/2 0s, return "1" else "0"
       (apply str))) ; combine into final rate string

(gamma-rate dummy-input)

(defn gamma-rate->epsilon-rate [s]
  (let [g->e {"1" "0"
              "0" "1"}]
    (->> s
         (map #(g->e (str %)))
         (apply str))))

(gamma-rate->epsilon-rate "10110")

(defn power-consumption [input]
  (let [g (gamma-rate input)
        e (gamma-rate->epsilon-rate g)]
    (* (Integer/parseInt e 2)
       (Integer/parseInt g 2))))

(power-consumption dummy-input)
(def input (read-string-seq "day3"))
(power-consumption input)


;; Part 2

; oxygen:
; - for each col, keep only nums with gamma rate at that digit
; - recalculate gamma rate for each new col, after filtering
; - (if 0 and 1 equally common, return 1)
; - keep going until only one num left: this is the oxygen rating

; co2 rating:
; - inverse of above
(defn- gamma-rate-bit [col]
  (if (<= (count (filter zero? col)) (/ (count col) 2))
    "1"
    "0"))

(defn- epsilon-rate-bit [col]
  (get {"1" "0"
        "0" "1"} (gamma-rate-bit col)))

(defn life-support-rates [input oxygen?]
  (let [num-digits-list (map (fn [n] (map #(Character/digit % 2) n)) input)] ; list of lists of ints
    (->> num-digits-list 
         (apply map (fn [& col] col))
         (reduce (fn [[i kept-indices] col]
                   (let [col (-> col
                                 vec
                                 (select-keys kept-indices)
                                 vals)
                         rate-bit (if oxygen?
                                    (gamma-rate-bit col)
                                    (epsilon-rate-bit col))
                         kept-indices (-> num-digits-list
                                          vec
                                          (select-keys kept-indices)
                                          (->> (filter (fn [[_index num-digits]]
                                                         (= rate-bit (str (nth num-digits i))))))
                                          keys)]
                     (if (= 1 (count kept-indices))
                       (reduced (nth num-digits-list (first kept-indices)))
                       [(inc i) kept-indices])))
                 [0 (range 0 (count num-digits-list))])
         (apply str))))

(life-support-rates dummy-input true)
(life-support-rates dummy-input false)

(defn life-support-rating [input]
  (let [o (life-support-rates input true)
        c (life-support-rates input false)]
    (* (Integer/parseInt o 2)
       (Integer/parseInt c 2))))

(life-support-rating input)
