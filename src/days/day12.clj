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
(defn- small-cave? [cave]
  (not= cave (.toUpperCase cave)))

(defn generate-legal-paths [graph]
  (letfn [(traverse [graph path visited?]
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


;; Part 2

; hmm... I guess in addition to a visited? set I need to keep track of how many times small caves have been visited
(defn generate-legal-paths' [graph]
  (letfn [(traverse [graph path visited? small-cave->visits]
            (let [cave (peek path)
                  small-cave->visits (if (small-cave? cave)
                                       (update small-cave->visits cave inc)
                                       small-cave->visits)
                  visit-limit? (some #(<= 2 %) (vals small-cave->visits))
                  visited? (if visit-limit?
                             (apply conj visited? (keep (fn [[cave visits]]
                                                          (when (pos? visits)
                                                            cave))
                                                        small-cave->visits))
                             visited?)
                  neighbours (remove visited? (get graph cave))]
              ;(prn path visited? small-cave->visits visit-limit?)
              (if (= "end" cave)
                [path]
                (mapcat (fn [neighbour]
                          (traverse
                            graph
                            (conj path neighbour)
                            visited?
                            small-cave->visits))
                        neighbours))))]
    (traverse graph ["start"] #{"start"} (zipmap
                                           (filter small-cave? (keys graph))
                                           (repeat 0)))))

(defn part-2 [input]
  (-> input
      parse-input
      generate-legal-paths'
      count))

(part-2 dummy-input-1) ; => 36
(part-2 dummy-input-2) ; => 103
(part-2 input) ; => 122880
