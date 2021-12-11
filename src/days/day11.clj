(ns days.day11
  (:require [clojure.pprint :refer [pprint]]

            [utils :refer [read-string-seq]]))

(def dummy-input-1 '("11111"
                     "19991"
                     "19191"
                     "19991"
                     "11111"))

(def dummy-input-2 '("5483143223"
                     "2745854711"
                     "5264556173"
                     "6141336146"
                     "6357385478"
                     "4167524645"
                     "2176841721"
                     "6882881134"
                     "4846848554"
                     "5283751526"))

(def input (read-string-seq "day11"))

(defn parse-input [input]
  (->> (sequence
         (comp
           (map (fn [s] (map #(Character/digit % 10) s)))
           (map-indexed (fn [row nums]
                          (map-indexed (fn [col n]
                                         [[row col] n])
                                       nums))))
         input)
       (apply concat) 
       (into {})))

(defn- prettify-output [location->energy-level]
  (->> (sort-by (fn [[k _v]] k) location->energy-level)
       (map (fn [[_k v]] v))
       (partition 10)))

(defn step [location->energy-level]
  (letfn [(get-neighbours [[row col]]
            (for [r (range (dec row) (+ row 2))
                  c (range (dec col) (+ col 2))
                  :when (and (<= 0 r) (> 10 r)
                             (<= 0 c) (> 10 c)
                             (not= [r c] [row col]) )]
              [r c]))
          (charge [location->energy-level]
            (->> (map (fn [[k v]]
                        [k (inc v)])
                      location->energy-level)
                 (into {})))
          (find-flashers [location->energy-level flashed]
            (keep (fn [[k v]]
                    (when (and (<= 10 v)
                               (nil? (flashed k)))
                      k))
                  location->energy-level))
          (reset-flashed [location->energy-level]
            (->> (map (fn [[k v]]
                        (if (<= 10 v)
                          [k 0]
                          [k v]))
                      location->energy-level)
                 (into {})))
          (charge-neighbours [location->energy-level neighbours]
            (->> (map (fn [[k v]]
                        [k (+ v (or (get neighbours k) 0))])
                      location->energy-level)
                 (into {})))]
    (-> (loop [location->energy-level (charge location->energy-level)
               flashed #{}]
          ;(pprint ["flashed" flashed])
          (let [flashers (find-flashers location->energy-level flashed)
                neighbours (frequencies (mapcat get-neighbours flashers))]
            ;(pprint ["neighbours" neighbours])
            ;(pprint ["flashers" flashers])
            (if (seq flashers)
              (recur (charge-neighbours location->energy-level neighbours)
                     (apply conj flashed flashers))
              (charge-neighbours location->energy-level neighbours))))
        reset-flashed)))

(defn n-step-flashes [location->energy-level n]
  (letfn [(count-flashes [l->e-l]
            (count (filter zero? (vals l->e-l))))]
    (loop [l->e-l location->energy-level
           total-flashes 0
           i 0]
      (if (< i n)
        (let [l->e-l (step l->e-l)]
          (recur
            l->e-l
            (+ total-flashes (count-flashes l->e-l))
            (inc i)))
        total-flashes))))

(defn part-1 [input]
  (-> input
      parse-input
      #_(->> (iterate step)
             (take 3)
             (map prettify-output))
      (n-step-flashes 100)))

;(part-1 dummy-input-1)
(part-1 dummy-input-2) ; => 1656
(part-1 input) ; => 1755


;; Part 2

(defn get-first-synched-flash-step [location->energy-level]
  (letfn [(synced? [l->e-l]
            (empty? (remove zero? (vals l->e-l))))]
    (loop [l->e-l (step location->energy-level)
           n 1]
      (if (synced? l->e-l)
        n
        (recur (step l->e-l) (inc n))))))

(defn part-2 [input]
  (-> input
      parse-input
      get-first-synched-flash-step))

(part-2 dummy-input-2) ; => 195
(part-2 input) ; => 212
