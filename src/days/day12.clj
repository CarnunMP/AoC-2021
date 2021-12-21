(ns days.day12
  (:require [clojure.string :as str]

            [utils :refer [read-string-seq]]))


(def dummy-input-1 '("start-A"
                     "start-b"
                     "A-c"
                     "A-b"
                     "b-d"
                     "A-end"
                     "b-end"))

(def dummy-input-2 '("dc-end"
                     "HN-start"
                     "start-kj"
                     "dc-start"
                     "dc-HN"
                     "LN-dc"
                     "HN-end"
                     "kj-sa"
                     "kj-HN"
                     "kj-dc"))

(def input (read-string-seq "day12"))


;; Part 1

(defn parse-input [input]
  (transduce
    (comp
      (map #(str/split % #"-"))
      (mapcat (fn [[a b]]
                [[a b] [b a]])))
    (completing
      (fn [m [a b]]
        (merge-with concat m {a (list b)})))
    {}
    input))

; recursive DFS!
(defn generate-legal-paths [graph]
  (letfn [(small-cave? [cave] (not= cave (.toUpperCase cave)))
          (traverse [graph path visited?]
            (let [cave (peek path)
                  neighbours (remove visited? (get graph cave))]
              (if (= "end" cave)
                [path]
                (mapcat (fn [neighbour]
                          (traverse
                            graph
                            (conj path neighbour)
                            (if (small-cave? cave)
                              (conj visited? cave)
                              visited?)))
                        neighbours))))]
    (traverse graph ["start"] #{"start"})))

(defn part-1 [input]
  (-> input
      parse-input
      generate-legal-paths
      count))

(part-1 dummy-input-1) ; => 10
(part-1 dummy-input-2) ; => 19
(part-1 input) ; => 3510
