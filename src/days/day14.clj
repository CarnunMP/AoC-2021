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


(comment
  ;; idea! don't need to produce intermediary strings and count them
  ;; can produce freqs of pairs directly, then derive final counts from final freqs of pairs
  ;; by producing freqs of second in pair +1 for first letter in template!!

  (defn parse [input]
    (let [[[template] _ rules] (partition-by str/blank? input)]
      [template (->> (map #(str/split % #" -> ") rules)
                     (map (fn [[[p1 p2] [res]]] [(list p1 p2) (list (list p1 res) (list res p2))]))
                     (into {}))]))

  (defn- next-pair-freqs [rules pair-freqs]
    (reduce-kv
      (fn [fs pair f]
        (let [[new-pair-1 new-pair-2] (rules pair)]
          (merge-with (fnil + 0) fs {new-pair-1 f
                                     new-pair-2 f})))
      {}
      pair-freqs))

  (defn- final-freqs [pair-freqs first-element]
    ;(prn pair-freqs)
    (-> (reduce-kv
          (fn [fs [_pair-1 pair-2] f]
            ;(prn pair-2 f)
            (update fs pair-2 (fnil + 0) f))
          {}
          pair-freqs)
        (update first-element inc)
        vals))

  (let [steps 40
        [[first-element :as template] rules] (parse input)
        pair-freqs (frequencies (partition 2 1 template))
        final-freqs (-> (partial next-pair-freqs rules)
                        (iterate pair-freqs)
                        (nth steps)
                        (final-freqs first-element))
        [min max] [(apply min final-freqs) (apply max final-freqs)]]
    (- max min))




  )
