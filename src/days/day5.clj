(ns days.day5
  (:require [clojure.string :as str]

            [utils :refer [read-string-seq]]))

(def dummy-input '("0,9 -> 5,9"
                   "8,0 -> 0,8"
                   "9,4 -> 3,4"
                   "2,2 -> 2,1"
                   "7,0 -> 7,4"
                   "6,4 -> 2,0"
                   "0,9 -> 2,9"
                   "3,4 -> 1,4"
                   "0,0 -> 8,8"
                   "5,5 -> 8,2"))

(def input (read-string-seq "day5"))


;; Part 1

(defn- parse-input
  "Returns a list of pairs of [x y] coordinates, i.e. line segments."
  [input]
  (prn "parse-input")
  (time
    (sequence
      (comp
        (map #(str/split % #" \-\> "))
        (map (fn [s] (map #(str/split % #",") s)))
        (map (fn [l] (map (fn [v] (mapv #(Integer/parseInt %) v)) l))))
      input)))

(defn- h&v-lines [line-segments]
  (prn "h&v-lines")
  (time
    (filter (fn [[[x1 y1] [x2 y2]]]
              (or (= x1 x2)
                  (= y1 y2)))
            line-segments)))

(defn- h&v-points
  "Assumes line-segments contains only horizontal and vertical lines."
  [line-segments]
  (prn "h&v-points")
  (time
    (mapcat
      (fn [[p1 p2]]
        (let [[[x1 y1] [x2 y2]] (sort [p1 p2])]
          (for [x (if (= x1 x2) [x1] (range x1 (inc x2)))
                y (if (= y1 y2) [y1] (range y1 (inc y2)))]
            [x y])))
      line-segments)))

(defn- point-freqs [points]
  (prn "point-freqs")
  (time
    (reduce
      (fn [freq-map point]
        ;(merge-with (fn [freq _] (inc freq)) {point 1} freq-map) - ah! <small map> <big map> was the problem!
        (merge-with (fn [freq _] (inc freq)) freq-map {point 1}))
      {}
      points)))

;(defn- point-freqs' [points]
;  (prn "point-freqs'")
;  (time
;    (reduce
;      (fn [freq-map p]
;        (if (get freq-map p)
;          (update freq-map p inc)
;          (assoc freq-map p 1)))
;      {}
;      points)))

(defn- danger-count [point-freqs]
  (prn "danger-count")
  (time
    (count
      (filter (fn [[_k v]]
                (<= 2 v))
              point-freqs))))

(defn part-1 [input]
  (time
    (-> input
        parse-input
        h&v-lines
        h&v-points
        point-freqs
        danger-count)))

(part-1 dummy-input) ; => 5
(part-1 input) ; => 7142


;; Part 2

(defn- h&v&d-lines [line-segments]
  (prn "h&v&d-lines")
  (time
    (filter (fn [[[x1 y1] [x2 y2]]]
              (or (= x1 x2)
                  (= y1 y2)
                  (= (Math/abs (- x2 x1)) (Math/abs (- y2 y1)))))
            line-segments)))

(defn- h&v&d-points [line-segments]
  (prn "h&v&d-points")
  (time
    (mapcat
      (fn [[p1 p2]]
        (let [[[x1 y1] [x2 y2]] (sort [p1 p2])
              diagonal? (= (Math/abs (- x2 x1)) (Math/abs (- y2 y1))) ]
          (if diagonal?
            (map (fn [x y] [x y])
                 (range x1 (inc x2))
                 (if (< y1 y2)
                   (range y1 (inc y2))
                   (range y1 (dec y2) -1)))  
            (for [x (if (= x1 x2) [x1] (range x1 (inc x2)))
                  y (if (= y1 y2) [y1] (range y1 (inc y2)))]
              [x y]))))
      line-segments)))

(defn part-2 [input]
  (time
    (-> input
        parse-input
        h&v&d-lines
        h&v&d-points
        point-freqs
        danger-count)))

(part-2 dummy-input) ; => 12
(part-2 input) ; => 20012
