(ns days.day7
  (:require [clojure.string :as str]

            [utils :refer [read-string-seq]]))


; Part 1

(def dummy-input "16,1,2,0,4,2,7,1,2,14")
(def input (first (read-string-seq "day7")))

(defn parse-input [input]
  (->> (str/split input #",")
       (map #(Integer/parseInt %))))

(defn assoc-median [nums]
  (let [nums (vec (sort nums))
        middle-index (dec (/ (count nums) 2))]
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
