(ns days.day10
  (:require [clojure.set :as set]

            [utils :refer [read-string-seq]]))


;; Part 1

(def dummy-input '("[({(<(())[]>[[{[]{<()<>>"
                   "[(()[<>])]({[<{<<[]>>("
                   "{([(<{}[<>[]}>{[]{[(<()>"
                   "(((({<>}<{<{<>}{[]{[]{}"
                   "[[<[([]))<([[{}[[()]]]"
                   "[{[{({}]{}}([{[{{{}}([]"
                   "{<[[]]>}<{[{[{[]{()[[[]"
                   "[<(<(<(<{}))><([]([]()"
                   "<{([([[(<>()){}]>(<<{{"
                   "<{([{{}}[<[[[<>{}]]]>[]]"))

(def input (read-string-seq "day10"))

(def l->r {\( \)
           \[ \]
           \{ \}
           \< \>})

(def r->l (set/map-invert l->r))

(def illegal-char->score {\) 3
                          \] 57
                          \} 1197
                          \> 25137})

(defn get-illegal-chars [input]
  (letfn [(illegal-char [s]
            (reduce
              (fn [stack c]
                (let [left (l->r c)]
                  (if left
                    (conj stack left)
                    (let [top-of-stack (first stack)]
                      (if (= c top-of-stack)
                        (rest stack)
                        (reduced c))))))
              '()
              s))]
    (->> (map illegal-char input)
         (remove seq?))))

(defn score [illegal-chars]
  (reduce
    (fn [total c]
      (+ total (illegal-char->score c)))
    0
    illegal-chars))

(defn part-1 [input]
  (-> input
      get-illegal-chars
      score))

(part-1 input) ; => 362271
