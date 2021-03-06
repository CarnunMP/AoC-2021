(ns days.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Part 1

(def dummy-instructions ["forward 5"
                         "down 5"
                         "forward 8"
                         "up 3"
                         "down 8"
                         "forward 2"])

(def instruction->dim-and-valence {"forward" [:horizontal true]
                                   "up" [:depth false]
                                   "down" [:depth true]})

(defn travel [instructions]
  (reduce
    (fn [position instruction-string]
      (let [[dim-string v-string] (str/split instruction-string #" ")
            [[dim positive?] v] [(instruction->dim-and-valence dim-string) (Integer/parseInt v-string)]]
        ;(prn dim-string v-string dim positive? v)
        (cond
          (= :horizontal dim)
          (update position :horizontal + v)
          
          (and (= :depth dim) positive?)
          (update position :depth + v)
          
          (and (= :depth dim) (not positive?))
          (update position :depth - v))))
    {:horizontal 0
     :depth 0}
    instructions))

(travel dummy-instructions)

(def instructions (with-open [r (io/reader "data/day2.txt")]
                    (doall (line-seq r))))

(->> (travel instructions)
     vals 
     (apply *))


;; Part 2

(defn travel* [instructions]
  (reduce
    (fn [{:keys [aim] :as position} instruction-string]
      (let [[dim-string v-string] (str/split instruction-string #" ")
            [[dim positive?] v] [(instruction->dim-and-valence dim-string) (Integer/parseInt v-string)]]
        ;(prn dim-string v-string dim positive? v)
        ;(prn position)
        (cond
          (= :horizontal dim)
          (-> position
              (update :horizontal + v)
              (update :depth #(+ % (* aim v))))
          
          (and (= :depth dim) positive?)
          (-> position
              ;(update :depth + v)
              (update :aim + v))
          
          (and (= :depth dim) (not positive?))
          (-> position
              ;(update :depth - v)
              (update :aim - v)))))
    {:horizontal 0
     :depth 0
     :aim 0}
    instructions))

(travel* dummy-instructions)
(travel* instructions)

(let [{:keys [horizontal depth]} (travel* instructions)]
  (* horizontal depth))


;; P.S.

(defn- parse [instruction-str]
  (let [[dim-str v-str] (str/split instruction-str #" ")]
    [dim-str (Integer/parseInt v-str)]))

(parse (first dummy-instructions))

(defn travel' [instructions & part-1?]
  (let [{:keys [h d]} (reduce
                        (fn [{:keys [a] :as pos} instruction-str]
                          (let [[dim-str v] (parse instruction-str)]
                            (case dim-str 
                              "forward" (cond-> pos
                                          (not part-1?) (update :d #(+ % (* a v)))
                                          :else (update :h + v))
                              "up" (if part-1?
                                     (update pos :d - v)
                                     (update pos :a - v))
                              "down" (if part-1?
                                       (update pos :d + v)
                                       (update pos :a + v)))))
                        {:h 0
                         :d 0
                         :a 0}
                        instructions)]
    (* h d)))

(travel' instructions true)
(travel' instructions)
