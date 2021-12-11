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

(defn get-illegal-or-completion-chars
  [input & {:keys [completion?] :or {completion? false}}]
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
    (cond->> (map illegal-char input)
      (not completion?) (remove seq?)
      completion? (remove char?))))

(defn score [illegal-chars]
  (reduce
    (fn [total c]
      (+ total (illegal-char->score c)))
    0
    illegal-chars))

(defn part-1 [input]
  (-> input
      get-illegal-or-completion-chars
      score))

(part-1 input) ; => 362271


;; Part 2

(def completion-char->score {\) 1
                             \] 2
                             \} 3
                             \> 4})

(defn score-completions [completions]
  (letfn [(score [cs]
            (reduce
              (fn [total c]
                (+ (* 5 total) (completion-char->score c)))
              0
              cs))]
    (map score completions)))

(defn get-middle-score [scores]
  (let [n (int (/ (count scores) 2))]
    (nth scores n)))

(defn part-2 [input]
  (-> input
      (get-illegal-or-completion-chars :completion? true)
      score-completions
      sort
      get-middle-score))

(part-2 dummy-input) ; => 288957
(part-2 input) ; => 1698395182
