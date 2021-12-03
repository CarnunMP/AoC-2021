(ns days.day1
  (:require [clojure.java.io :as io]))

;; Part 1

(def dummy-depths '(199
                    200
                    208
                    210
                    200
                    207
                    240
                    269
                    260
                    263))

(def depth-xform
  (comp
    (map #(< %1 %2))
    (filter true?)))

(defn depth-counts [depths]
  (count (sequence
           depth-xform
           depths (rest depths))))

(depth-counts dummy-depths)

(def depths (with-open [r (io/reader "data/day1.txt")]
              (doall (map #(Integer/parseInt %) (line-seq r)))))

(depth-counts depths)

;; Part 2

(defn depth-counts* [depths]
  (->> (map #(+ %1 %2 %3)
         depths (rest depths) (rest (rest depths)))
       depth-counts))

(depth-counts* dummy-depths)
(depth-counts* depths)
