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
      (cond-> {:location [row col]
               :value (-> locations (get row) (get col))}
        (> row 0) (assoc-in [:neighbours :up] {:location [(dec row) col]
                                               :value (-> locations (get (dec row)) (get col))})
        (< row (dec rows)) (assoc-in [:neighbours :down] {:location [(inc row) col]
                                                          :value (-> locations (get (inc row)) (get col))})
        (> col 0) (assoc-in [:neighbours :left] {:location [row (dec col)]
                                                 :value (-> locations (get row) (get (dec col)))})
        (< col (dec cols)) (assoc-in [:neighbours :right] {:location [row (inc col)]
                                                           :value (-> locations (get row) (get (inc col)))})))))

(defn low-points [locations+neighbours]
  (reduce
    (fn [low-points {:keys [value neighbours] :as m}]
      (if (every? #(< value %) (map :value (vals neighbours)))
        (conj low-points m)
        low-points))
    '()
    locations+neighbours))

(defn risk-levels-sum [low-points]
  (reduce
    (fn [sum {:keys [value]}]
      (+ sum 1 value))
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


;; Part 2

; a basin is basically: flood fill from low points until you hit 9s

(defn location->value+neighbours [locations+neighbours]
  (into {} (map (fn [{:keys [location value neighbours]}]
                  [location {:value value
                             :neighbours neighbours}])
                locations+neighbours)))

(defn- fill-basin [low-point-location location->value+neighbours]
  (loop [visited #{}
         q (conj (clojure.lang.PersistentQueue/EMPTY) low-point-location)]
    (let [current-location (peek q)
          visited (conj visited current-location)
          q (apply conj (pop q) (->> (location->value+neighbours current-location)
                                     :neighbours
                                     vals
                                     (filter #(and (not= 9 (:value %))
                                                   (not (visited (:location %)))))
                                     (map :location)))]
      (if (seq q)
        (recur visited q)
        visited))))

(defn basins [low-points location->value+neighbours]
  (map #(fill-basin % location->value+neighbours) (map :location low-points)))

(defn part-2 [input]
  (let [locations+neighbours (-> input
                                 parse-input
                                 neighbours)
        low-points (low-points locations+neighbours)
        location->value+neighbours (location->value+neighbours locations+neighbours)
        basins (basins low-points location->value+neighbours)]
    (->> (sort-by count basins)
         (take-last 3)
         (reduce #(* %1 (count %2)) 1))))

(part-2 dummy-input) ; => 1134
(part-2 input) ; => 1075536
