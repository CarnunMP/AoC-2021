(ns days.day13
  (:require [clojure.string :as str]

            [utils :refer [read-string-seq]]))

(def dummy-input '("6,10"
                   "0,14"
                   "9,10"
                   "0,3"
                   "10,4"
                   "4,11"
                   "6,0"
                   "6,12"
                   "4,1"
                   "0,13"
                   "10,12"
                   "3,4"
                   "3,0"
                   "8,4"
                   "1,10"
                   "2,14"
                   "8,10"
                   "9,0"
                   ""
                   "fold along y=7"
                   "fold along x=5"))

(def input (read-string-seq "day13"))


;; Part 1

(defn parse-input [input]
  (let [[dots _ folds] (partition-by str/blank? input)]
    {:dots (sequence
             (comp (map #(str/split % #","))
                   (map (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)])))
             dots)
     :folds (sequence
              (comp (map #(str/split % #" "))
                    (map (fn [[_ _ line]]
                           (let [[axis v] (str/split line #"=")]
                             [axis (Integer/parseInt v)]))))
              folds)}))

(defn fold-all [{:keys [dots folds]} & {:keys [n]}]
  (letfn [(fold [dots [axis v]]
            (let [{moving true
                   still false} (group-by (fn [[x y]]
                                            (if (= "y" axis)
                                              (< v y)
                                              (< v x)))
                                          dots)]
              (apply conj
                     (set still)
                     (map (fn [[x y]]
                            (if (= "y" axis)
                              [x (- y (* 2 (- y v)))]
                              [(- x (* 2 (- x v))) y]))
                          moving))))]
    (loop [dots dots
           current-fold (first folds)
           folds (rest folds)
           i 1]
      (if (or (empty? folds) (= i n))
        (fold dots current-fold)
        (recur
          (fold dots current-fold)
          (first folds)
          (rest folds)
          (inc i))))))

(defn part-1 [input]
  (-> input
      parse-input
      (fold-all :n 1)
      count))

(part-1 dummy-input) ; => 17
(part-1 input) ; => 720


;; Part 2

(defn display [dots]
  (let [max-x (apply max (map first dots))
        max-y (apply max (map second dots))]
    (->> (for [x (range (inc max-x))
               y (range (inc max-y))]
           (if (dots [x y])
             [[x y] "█"]
             [[x y] " "]))
         (group-by (fn [[[_x y] _v]] y))
         vals
         (map (fn [row] (map second row)))
         (map #(apply str %)))))

(defn part-2 [input]
  (-> input
      parse-input
      fold-all
      display))

(part-2 input)
;(" ██  █  █ ███  ███  ███   ██  █  █ ████"
; "█  █ █  █ █  █ █  █ █  █ █  █ █  █    █"
; "█  █ ████ █  █ █  █ █  █ █  █ █  █   █ "
; "████ █  █ ███  ███  ███  ████ █  █  █  "
; "█  █ █  █ █    █ █  █    █  █ █  █ █   "
; "█  █ █  █ █    █  █ █    █  █  ██  ████")
