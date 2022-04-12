(ns days.day15
  (:require [utils :refer [read-string-seq]]
            [ubergraph.core :as uber.core]
            [ubergraph.alg :as uber.alg]))


(def dummy-input '("1163751742"
                   "1381373672"
                   "2136511327"
                   "3694931569"
                   "7463417111"
                   "1319128137"
                   "1359912421"
                   "3125421639"
                   "1293138521"
                   "2311944581"))

(def input (read-string-seq "day15"))


(defn- parse [input]
  (mapv (fn [s]
          (mapv (fn [c]
                  (Character/digit c 10))
                s))
        input))

(defn- edges [risks]
  (let [size (count risks)]
    (->> (for [row (range size)
               col (range size)
               :let [neighbours (for [r-off [-1 0 1]
                                      c-off [-1 0 1]
                                      :when (and (<= 0 (+ row r-off)) (> size (+ row r-off))
                                                 (<= 0 (+ col c-off)) (> size (+ col c-off))
                                                 (not= (Math/abs r-off) (Math/abs c-off)))]
                                  [(+ row r-off) (+ col c-off)])]]
           (map (fn [neighbour]
                  [[row col] neighbour (get-in risks neighbour)])
                neighbours))
         (apply concat))))

(defn lowest-total-risk [input]
  (let [g (uber.core/multidigraph)
        size (count input)]
    (-> input
        parse
        edges
        (->> (uber.core/add-edges* g))
        (uber.alg/shortest-path [0 0] [(dec size) (dec size)] :weight)
        uber.alg/cost-of-path)))

(comment
  (lowest-total-risk dummy-input) ; => 40

  ;; part 1
  (lowest-total-risk input)

  )
