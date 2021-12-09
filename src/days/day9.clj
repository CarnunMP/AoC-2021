(ns days.day9
  (:require [utils :refer [read-string-seq]]))

;; Part 1

(def dummy-input '("2199943210"
                   "3987894921"
                   "9856789892"
                   "8767896789"
                   "9899965678"))
(def input (read-string-seq "day9"))

(defn parse-input [input]
  (mapv (fn [s]
          (mapv #(Character/digit % 10) s))
        input))

(defn neighbours [locations]
  (let [rows (count locations)
        cols (count (first locations))]
    (for [row (range rows)
          col (range cols)]
      (cond-> {;:location [row col]
               :value (-> locations (get row) (get col))}
        (> row 0) (assoc-in [:neighbours :up] (-> locations (get (dec row)) (get col)))
        (< row (dec rows)) (assoc-in [:neighbours :down] (-> locations (get (inc row)) (get col)))
        (> col 0) (assoc-in [:neighbours :left] (-> locations (get row) (get (dec col))))
        (< col (dec cols)) (assoc-in [:neighbours :right] (-> locations (get row) (get (inc col))))))))

(defn low-points [locations+neighbours]
  (reduce
    (fn [low-points {:keys [value neighbours]}]
      (if (every? #(< value %) (vals neighbours))
        (conj low-points value)
        low-points))
    '()
    locations+neighbours))

(defn risk-levels-sum [low-points]
  (reduce
    (fn [sum point]
      (+ sum 1 point))
    0
    low-points))

(defn part-1 [input]
  (-> input
      parse-input
      neighbours
      low-points
      risk-levels-sum))

(part-1 dummy-input) ; => 15
(part-1 input) ; => 491
