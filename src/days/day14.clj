(ns days.day14
  (:require [clojure.string :as str]
            [utils :refer [read-string-seq]]))


(def dummy-input '("NNCB"
                   ""
                   "CH -> B"
                   "HH -> N"
                   "CB -> H"
                   "NH -> C"
                   "HB -> C"
                   "HC -> B"
                   "HN -> C"
                   "NN -> C"
                   "BH -> H"
                   "NC -> B"
                   "NB -> B"
                   "BN -> B"
                   "BB -> N"
                   "BC -> B"
                   "CC -> N"
                   "CN -> C"))

(def input (read-string-seq "day14"))


(defn parse [input]
  (let [[[template] _ rules] (partition-by str/blank? input)]
    [template (->> (map #(str/split % #" -> ") rules)
                   (map (fn [[[p1 p2] [res]]] [[p1 p2] [[p1 res] [res p2]]]))
                   (into {}))]))


(defn- next-pair-freqs [rules pair-freqs]
  (reduce-kv
    (fn [fs pair f]
      (let [[new-pair-1 new-pair-2] (rules pair)]
        (merge-with (fnil + 0) fs {new-pair-1 f
                                   new-pair-2 f})))
    {}
    pair-freqs))

(defn- final-freqs
  "Like next-pair-freqs, except ignores the first element in each pair (to dedupe them).
  Returns a list of just the frequency values."
  [pair-freqs first-element]
  (-> (reduce-kv
        (fn [fs [_pair-1 pair-2] f]
          (update fs pair-2 (fnil + 0) f))
        {}
        pair-freqs)
      (update first-element inc) ; don't ignore first element in template!
      vals))

(defn polymerize [input steps]
  (let [[[first-element :as template] rules] (parse input)
        pair-freqs (frequencies (partition 2 1 template))
        final-freqs (-> (partial next-pair-freqs rules)
                        (iterate pair-freqs)
                        (nth steps)
                        (final-freqs first-element))
        [min max] [(apply min final-freqs) (apply max final-freqs)]]
    (- max min)))

(comment
  ;; part 1
  (polymerize input 10) ; => 2712

  ;; part 2
  (polymerize input 40) ; => 8336623059567 

  )
